import torch
import argparse
from pyhocon import ConfigFactory
import random

from pytorch.taskManager import TaskManager
from pytorch.metal import Metal
from pytorch.utils import *
from pytorch.constEmbeddingsGlove import ConstEmbeddingsGlove
from sequences.rowReaders import *
from pytorch.seqScorer import *

import onnx
import onnxruntime

import json

def to_numpy(tensor):
    return tensor.detach().cpu().numpy() if tensor.requires_grad else tensor.cpu().numpy()

class Char_RNN(torch.nn.Module):

    def __init__(self, model):
        super().__init__()
        for i, layers in enumerate(model):
            if layers.initialLayer is not None:
                self.char_lookup = layers.initialLayer.charLookupParameters
                self.char_rnn = layers.initialLayer.charRnnBuilder

    def forward(self, char_ids):
        charEmbedding = mkCharacterEmbedding2(char_ids, self.char_lookup, self.char_rnn)
        return charEmbedding

class Saving_Model(torch.nn.Module):
    """docstring for Saving_Model"""
    def __init__(self, model):
        super().__init__()
        self.model_length = len(model)
        self.intermediateLayerss = [None for _ in range(self.model_length)]
        self.finalLayers = [None for _ in range(self.model_length)]
        for i, layers in enumerate(model):
            if layers.initialLayer is not None:
                self.word_lookup = layers.initialLayer.wordLookupParameters
                self.postag_lookup = layers.initialLayer.posTagLookupParameters
                self.netag_lookup = layers.initialLayer.neTagLookupParameters
                self.dist_lookup = layers.initialLayer.distanceLookupParameters
                self.pos_lookup = layers.initialLayer.positionLookupParameters
                self.useIsPredicate = layers.initialLayer.useIsPredicate
                self.distanceWindowSize = layers.initialLayer.distanceWindowSize
            self.intermediateLayerss[i] = nn.ModuleList(layers.intermediateLayers)
            self.finalLayers[i] = layers.finalLayer
        self.intermediateLayerss = nn.ModuleDict({str(i):t for i, t in enumerate(self.intermediateLayerss)})
        self.finalLayers = nn.ModuleDict({str(i):t for i, t in enumerate(self.finalLayers)})
    def forward(self, embeddings, word_ids, charEmbedding, tags, nes, predEmbed, dists, headPositions):
        # Can I assuem there is only one initial layer?
        learnedWordEmbeddings = self.word_lookup(word_ids)
        posTagEmbed = self.postag_lookup(tags)
        neTagEmbed = self.netag_lookup(nes)
        distanceEmbedding = self.dist_lookup(dists)
        embedParts = [embeddings, learnedWordEmbeddings, charEmbedding, posTagEmbed, neTagEmbed, distanceEmbedding, predEmbed]
        
        state = torch.cat(embedParts, dim=1)
        for i in range(self.model_length):
            i = str(i)
            for il in self.intermediateLayerss[i]:
                state = il(state)
            if self.finalLayers[i]:
                state = self.finalLayers[i](state, headPositions)#headPositions set to be None for now, we can add it in input list later
        ids = self.finalLayers[str(self.model_length-1)].inference2(state)
        return ids

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--model_file', type=str, help='Filename of the model.', nargs='+')
    parser.add_argument('--config', type=str, help='Filename of the configuration.')
    parser.add_argument('--seed', type=int, default=1234)
    args = parser.parse_args()

    config = ConfigFactory.parse_file(f'../resources/org/clulab/{args.config}.conf')
    taskManager = TaskManager(config, args.seed)
    modelName = args.model_file
    if len(modelName)==1:
        model = Metal.load(modelName[0])
    else:
        model = Metal.load_multi(modelName)

    for layers in model:
        layers.start_eval()
    constEmbeddings = ConstEmbeddingsGlove.get_ConstLookupParams()

    export_char  = Char_RNN(model)
    export_model = Saving_Model(model)
    export_model.eval()
    export_char.eval()
    for param in export_model.parameters():
        param.requires_grad = False
    for param in export_char.parameters():
        param.requires_grad = False

    torch.manual_seed(taskManager.random)
    random.seed(taskManager.random)

    x2i = json.load(open(args.model_file[0]+".json"))

    c2i = x2i[0]['x2i']['initialLayer']['c2i']
    w2i = x2i[0]['x2i']['initialLayer']['w2i']
    t2i = x2i[0]['x2i']['initialLayer']['tag2i']
    n2i = x2i[0]['x2i']['initialLayer']['ne2i']

    useIsPredicate = x2i[0]['x2i']['initialLayer']['useIsPredicate']
    distanceWindowSize = x2i[0]['x2i']['initialLayer']['distanceWindowSize']
    positionEmbeddingSize = x2i[0]['x2i']['initialLayer']['positionEmbeddingSize']

    for taskId in range(0, taskManager.taskCount):
        taskName = taskManager.tasks[taskId].taskName
        testSentences = taskManager.tasks[taskId].testSentences
        if testSentences:
            reader = MetalRowReader()
            annotatedSentences = reader.toAnnotatedSentences(testSentences[0])

            asent = annotatedSentences[0]
            sentence = asent[0]
            goldLabels = asent[1]

            words = sentence.words
            tags = sentence.posTags
            nes = sentence.neTags
            headPositions = torch.LongTensor(sentence.headPositions)
            char_embs = []
            for word in words:
                char_ids = torch.LongTensor([c2i.get(c, UNK_EMBEDDING) for c in word])
                char_out = export_char(char_ids)
                char_embs.append(char_out)
            char_embs = torch.stack(char_embs)
            embed_ids = torch.LongTensor([constEmbeddings.w2i[word] if word in constEmbeddings.w2i else 0 for word in words])
            embeddings = constEmbeddings.emb(embed_ids)
            word_ids = torch.LongTensor([w2i[word] if word in w2i else 0 for word in words])
            tags_ids = torch.LongTensor([t2i[tag] if tag in t2i else 0 for tag in tags])
            nes_ids = torch.LongTensor([n2i[ne] if ne in n2i else 0 for ne in nes])
            pred_embs = torch.FloatTensor([1 if i==predicatePosition else 0 for i, predicatePosition in enumerate(headPositions)]).unsqueeze(1)
            dists = [max(i-predicatePosition+distanceWindowSize+1, 0) if i-predicatePosition <= distanceWindowSize else 2 * distanceWindowSize + 2 for i, predicatePosition in enumerate(headPositions)]
            dists = torch.LongTensor(dists)
            
            output = export_model(embeddings, word_ids, char_embs, tags_ids, nes_ids, pred_embs, dists, headPositions)

            dummy_input = (embeddings, word_ids, char_embs, tags_ids, nes_ids, pred_embs, dists, headPositions)
            
    torch.onnx.export(export_char,
                    char_ids,
                    "char.onnx",
                    export_params=True,
                    do_constant_folding=True,
                    input_names = ['char_ids'],
                    output_names = ['chars'],
                    dynamic_axes = {"char_ids": {0: 'word length'}})

    torch.onnx.export(torch.jit.script(export_model),               # model being run
                  dummy_input,                         # model input (or a tuple for multiple inputs)
                  "model.onnx",   # where to save the model (can be a file or file-like object)
                  export_params=True,        # store the trained parameter weights inside the model file
                  opset_version=10,          # the ONNX version to export the model to
                  do_constant_folding=True,  # whether to execute constant folding for optimization
                  input_names  = ['embed', 'words', 'chars', 'tags', 'nes', 'pred_embs', 'dists', 'headPositions'],   # the model's input names
                  output_names = ['output'], # the model's output names
                  dynamic_axes = {'embed' : {0 : 'sentence length'},
                                  'words' : {0 : 'sentence length'},
                                  'chars' : {0 : 'sentence length'},
                                  'tags' : {0 : 'sentence length'},
                                  'nes' : {0 : 'sentence length'},
                                  'pred_embs' : {0 : 'sentence length'},
                                  'dists' : {0 : 'sentence length'},
                                  'headPositions' : {0 : 'sentence length'},
                                  'output': {0 : 'sentence length'}})

    onnx_model = onnx.load("model.onnx")
    onnx.checker.check_model(onnx_model)
    char_model = onnx.load("char.onnx")
    onnx.checker.check_model(char_model)

    ort_session = onnxruntime.InferenceSession("model.onnx")
    ort_char = onnxruntime.InferenceSession("char.onnx")
    # compute ONNX Runtime output prediction

    ort_inputs = {ort_char.get_inputs()[i].name: to_numpy(x) for i, x in enumerate([char_ids])}
    ort_outs = ort_char.run(None, ort_inputs)
    try:
        np.testing.assert_allclose(to_numpy(char_out), ort_outs[0], rtol=1e-03, atol=1e-05)
    except AssertionError as e:
        print (e)
    print ([i.name for i in ort_session.get_inputs()])
    ort_inputs = {ort_session.get_inputs()[i].name: to_numpy(x) for i, x in enumerate(dummy_input)}
    ort_outs = ort_session.run(None, ort_inputs)
    try:
        np.testing.assert_allclose(output.detach().cpu().numpy(), ort_outs[0], rtol=1e-03, atol=1e-05)
    except AssertionError as e:
        print (e)

    print("Exported model has been tested with ONNXRuntime, and the result looks good!")

    