package org.clulab.processors.clu.backend

import ai.onnxruntime.OnnxTensor
import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import org.clulab.dynet.Utils
import org.clulab.processors.clu.AnnotatedSentence
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.FileUtils
import org.json4s.JArray
import org.json4s.JInt
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.parse

import java.io.BufferedInputStream
import _root_.scala.io.Source

object OnnxBackend extends CluBackend

class OnnxPosBackend(modelFilenamePrefix: String) extends PosBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) = ???  // tags, chunks, and preds
}

class OnnxNerBackend(wordModel: String, charModel: String, x2i: String) extends NerBackend {

  def get_embeddings(embed_file_path: String): Map[String,Array[Float]]={
    val emb = Source.fromFile(embed_file_path)
    var emb_map: Map[String,Array[Float]] = Map()
    for (s <- emb.getLines) {
      if (s.split(" ")(0) == ""){
        emb_map += ("<UNK>"-> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
      }else{
        emb_map += (s.split(" ")(0) -> s.split(" ").slice(1, s.split(" ").size).map(_.toFloat))
      }
    }
    emb_map
  }

  val (w2i, c2i, i2t) = {

    def toMap(jValue: JValue): Map[String, Long] = {
      jValue.asInstanceOf[JObject].obj.map { case (key: Any, value) =>
        key -> value.asInstanceOf[JInt].num.longValue()
      }.toMap
    }

    val json = FileUtils.getTextFromResource(x2i)
    val jArray = parse(json).asInstanceOf[JArray].arr
    val w2i = toMap((jArray(0) \ "x2i" \ "initialLayer" \ "w2i"))
    val c2i = toMap((jArray(0) \ "x2i" \ "initialLayer" \ "c2i"))
    val t2i = toMap((jArray(1) \ "x2i" \ "finalLayer"   \ "t2i"))
    val i2t = t2i.map { case (key, value) => value -> key }
    // TODO: Turn this into an indexedseq and just get at the right index

    (w2i, c2i, i2t)
  }

  val ortEnvironment = OrtEnvironment.getEnvironment
  val sessionCreator = new SessionCreator(ortEnvironment)
  val wordSession = sessionCreator.create(wordModel)
  val charSession = sessionCreator.create(charModel)

  val embed_file_path: String = "../glove/glove.840B.300d.10f.txt"
  val wordEmbeddingMap = get_embeddings(embed_file_path)

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = {
    val words = annotatedSentence.words
    val embeddings: Array[Array[Float]] = new Array[Array[Float]](words.length)
    val wordIds: Array[Long] = new Array[Long](words.length)
    val char_embs: Array[Array[Float]] = new Array[Array[Float]](words.length)

    for (i <- words.indices){
      val word = words(i)
      embeddings(i) = wordEmbeddingMap.getOrElse(word,wordEmbeddingMap.get( "<UNK>").get)
      wordIds(i) = w2i.getOrElse(word, 0).asInstanceOf[Number].longValue
      val char_input = new java.util.HashMap[String, OnnxTensor]()
      char_input.put("char_ids",  OnnxTensor.createTensor(ortEnvironment, word.map(c => c2i.getOrElse(c.toString, 0).asInstanceOf[Number].longValue).toArray))
      char_embs(i) = charSession.run(char_input).get(0).getValue.asInstanceOf[Array[Float]]
    }

    val input = new java.util.HashMap[String, OnnxTensor]()
    val emb_tensor = OnnxTensor.createTensor(ortEnvironment, embeddings)
    input.put("embed", emb_tensor)
    val word_tensor = OnnxTensor.createTensor(ortEnvironment, wordIds)
    input.put("words", word_tensor)
    val char_tensor = OnnxTensor.createTensor(ortEnvironment, char_embs)
    input.put("chars", char_tensor)
    val emissionScores = wordSession.run(input).get(0).getValue.asInstanceOf[Array[Array[Float]]]
    val labelIds = Utils.greedyPredict(emissionScores)
    val preds = labelIds.map(i2t(_))

    preds
  }
}

class OnnxSrlaBackend(modelFilenamePrefix: String) extends SrlaBackend {

  def predict(taskId: Int, annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[String] = ???
}

class OnnxDepsBackend(modelFilenamePrefix: String) extends DepsBackend {

  def predict(annotatedSentence: AnnotatedSentence, embeddingsAttachment: EmbeddingsAttachment):
      IndexedSeq[(Int, String)] = ??? // heads and labels
}

class SessionCreator(ortEnvironment: OrtEnvironment) {

  // See https://stackoverflow.com/questions/33755415/how-to-read-a-resource-file-to-a-byte-array-in-scala
  def getBytesFromResource(resourceName: String): Array[Byte] = {
    new BufferedInputStream(getClass.getResourceAsStream(resourceName)).autoClose { inputStream =>
      Stream.continually(inputStream.read).takeWhile(-1 !=).map(_.toByte).toArray
    }
  }

  def create(resourceName: String): OrtSession = {
    val model = getBytesFromResource(resourceName)
    val session = ortEnvironment.createSession(model, new OrtSession.SessionOptions)

    session
  }
}
