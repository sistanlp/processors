package org.clulab.processors.clu

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.numeric.{NumericEntityRecognizer, setLabelsAndNorms}
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.processors.clu.tokenizer._
import org.clulab.scala.WrappedArray._
import org.clulab.scala_transformers.encoder.TokenClassifier
import org.clulab.scala_transformers.encoder.EncoderMaxTokensRuntimeException
import org.clulab.sequences.{LexiconNER, NamedEntity}
import org.clulab.struct.DirectedGraph
import org.clulab.struct.GraphMap
import org.clulab.utils.{Configured, MathUtils, ToEnhancedDependencies}
import org.slf4j.{Logger, LoggerFactory}

import org.clulab.odin.Mention

import BalaurProcessor._
import PostProcessor._
import org.clulab.processors.hexatagging.HexaDecoder

class BalaurProcessor protected (
  val config: Config,
  val optionalNER: Option[LexiconNER],
  val numericEntityRecognizerOpt: Option[NumericEntityRecognizer],
  wordTokenizer: Tokenizer,
  wordLemmatizer: Lemmatizer,
  tokenClassifier: TokenClassifier // multi-task classifier for all tasks addressed
) extends Processor with Configured {
  // This comes from scala-transformers, so we can't make a class from it here.
  type PredictionScore = (String, Float)

  // standard, abbreviated constructor
  def this(
    config: Config = ConfigFactory.load("balaurprocessor"),
    optionalNER: Option[LexiconNER] = None,
    seasonPathOpt: Option[String] = Some("/org/clulab/numeric/SEASON.tsv")
  ) = this(
    config,
    optionalNER,
    newNumericEntityRecognizerOpt(seasonPathOpt),
    mkTokenizer(BalaurProcessor.getArgString(config, s"$prefix.language", Some("EN"))),
    mkLemmatizer(BalaurProcessor.getArgString(config, s"$prefix.language", Some("EN"))),
    // TokenClassifier.fromFiles(config.getString(s"$prefix.modelName"))
    TokenClassifier.fromResources(config.getString(s"$prefix.modelName"))
  )

  def copy(
    configOpt: Option[Config] = None,
    optionalNEROpt: Option[Option[LexiconNER]] = None,
    numericEntityRecognizerOptOpt: Option[Option[NumericEntityRecognizer]] = None,
    wordTokenizerOpt: Option[Tokenizer] = None,
    wordLemmatizerOpt: Option[Lemmatizer] = None,
    tokenClassifierOpt: Option[TokenClassifier] = None
  ): BalaurProcessor = {
    new BalaurProcessor(
      configOpt.getOrElse(this.config),
      optionalNEROpt.getOrElse(this.optionalNER),
      numericEntityRecognizerOptOpt.getOrElse(this.numericEntityRecognizerOpt),
      wordTokenizerOpt.getOrElse(this.wordTokenizer),
      wordLemmatizerOpt.getOrElse(this.wordLemmatizer),
      tokenClassifierOpt.getOrElse(this.tokenClassifier)
    )
  }

  val hexaDecoder = new HexaDecoder()

  override def getConf: Config = config

  override def mkDocument(text: String, keepText: Boolean): Document = { 
    DocumentMaker.mkDocument(tokenizer, text, keepText)
  }

  def tokenizer: Tokenizer = wordTokenizer

  override def mkDocumentFromSentences(sentences: Iterable[String], 
    keepText: Boolean, 
    charactersBetweenSentences: Int): Document = {     
    DocumentMaker.mkDocumentFromSentences(tokenizer, sentences, keepText, charactersBetweenSentences)
  }

  override def mkDocumentFromTokens(sentences: Iterable[Iterable[String]], 
    keepText: Boolean, 
    charactersBetweenSentences: Int, 
    charactersBetweenTokens: Int): Document = { 
    DocumentMaker.mkDocumentFromTokens(sentences, keepText, charactersBetweenSentences, charactersBetweenSentences)
  }

  override def tagPartsOfSpeech(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its own in this processor!")
  }

  /** Lematization; modifies the document in place */
  override def lemmatize(doc: Document): Unit = {
    for(sent <- doc.sentences) {
      val lemmas = new Array[String](sent.size)
      for(i <- sent.words.indices) {
        lemmas(i) = wordLemmatizer.lemmatizeWord(sent.words(i))

        // a lemma may be empty in some weird Unicode situations
        if(lemmas(i).isEmpty) {
          logger.debug(s"""WARNING: Found empty lemma for word #$i "${sent.words(i)}" in sentence: ${sent.words.mkString(" ")}""")
          lemmas(i) = sent.words(i).toLowerCase()
        }
      }
      sent.lemmas = Some(lemmas)
    }
  }

  /** Generates cheap lemmas with the word in lower case, for languages where a lemmatizer is not available */
  def cheapLemmatize(doc:Document): Unit = {
    for(sent <- doc.sentences) {
      val lemmas = sent.words.map(_.toLowerCase()).toArray
      sent.lemmas = Some(lemmas)
    }
  }

  override def recognizeNamedEntities(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its own in this procecessor!")
  }

  override def parse(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its own in this procecessor!")
  }

  override def srl(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def chunking(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its own in this procecessor!")
  }

  override def resolveCoreference(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def discourse(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def relationExtraction(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def annotate(doc: Document): Document = {
    val verbose = false

    // lemmas are created deterministically, not through the MTL framework
    lemmatize(doc)

    // process one sentence at a time through the MTL framework
    for (sent <- doc.sentences) {
      try {
        val allLabelsAndScores = tokenClassifier.predictWithScores(sent.words)
        assignPosTags(allLabelsAndScores(TASK_TO_INDEX(POS_TASK)), sent)
        assignNamedEntityLabels(allLabelsAndScores(TASK_TO_INDEX(NER_TASK)), sent)
        assignChunkLabels(allLabelsAndScores(TASK_TO_INDEX(CHUNKING_TASK)), sent)
        assignDependencyLabelsUsingHexaTags(
          allLabelsAndScores(TASK_TO_INDEX(HEXA_TERM_TASK)), 
          allLabelsAndScores(TASK_TO_INDEX(HEXA_NONTERM_TASK)), 
          sent
        )
      } catch {
        case e: EncoderMaxTokensRuntimeException => 
          // this sentence exceeds the maximum number of tokens for the encoder
          // TODO: at some point do something smart here
          println(s"ERROR: this sentence exceeds the maximum number of tokens for the encoder and will not be annotated: ${sent.words.mkString(" ")}")

      }
    }

    // numeric entities using our numeric entity recognizer based on Odin rules
    if(numericEntityRecognizerOpt.nonEmpty) {
      val numericMentions = extractNumericEntityMentions(doc)
      setLabelsAndNorms(doc, numericMentions)
    }

    doc
  }

  def extractNumericEntityMentions(doc:Document): Seq[Mention] = {
    numericEntityRecognizerOpt.get.extractFrom(doc)
  }

  private def assignPosTags(labels: Array[Array[(String, Float)]], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)
    sent.tags = Some(postprocessPartOfSpeechTags(sent.words, labels.map(_.head._1).toArray))
  }

  /** Must be called after assignPosTags and lemmatize because it requires Sentence.tags and Sentence.lemmas */
  private def assignNamedEntityLabels(labels: Array[Array[(String, Float)]], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)

    // NER labels from the custom NER
    val optionalNERLabels: Option[Array[String]] = optionalNER.map { ner =>
      val sentence = Sentence(
        sent.words,
        sent.startOffsets,
        sent.endOffsets,
        sent.words,
        sent.tags,
        sent.lemmas,
        entities = None,
        norms = None,
        chunks = None,
        tree = None,
        deps = EMPTY_GRAPH,
        relations = None
      )

      ner.find(sentence)
    }

    val genericLabels = NamedEntity.patch(labels.map(_.head._1).toArray)

    if(optionalNERLabels.isEmpty) {
      sent.entities = Some(genericLabels)
    } else {
      //println(s"MERGING NE labels for sentence: ${sent.words.mkString(" ")}")
      //println(s"Generic labels: ${NamedEntity.patch(labels).mkString(", ")}")
      //println(s"Optional labels: ${optionalNERLabels.get.mkString(", ")}")
      val mergedLabels = NamedEntity.patch(mergeNerLabels(genericLabels, optionalNERLabels.get))
      //println(s"Merged labels: ${mergedLabels.mkString(", ")}")
      sent.entities = Some(mergedLabels)
    }
  }

  private def mergeNerLabels(generic: Array[String], custom: Array[String]): Array[String] = {
    require(generic.length == custom.length)

    val customNamedEntities = NamedEntity.collect(custom)
    val result = generic.toArray // A copy of the generic labels is created here.

    if (customNamedEntities.isEmpty)
      result
    else {
      val genericNamedEntities = NamedEntity.collect(generic)

      //println(s"Generic NamedEntity: ${genericNamedEntities.mkString(", ")}")
      //println(s"Custom NamedEntity: ${customNamedEntities.mkString(", ")}")

      // The custom labels override the generic ones!
      NamedEntity.combine(result, genericNamedEntities, customNamedEntities)
    }
  }

  private def assignChunkLabels(labels: Array[Array[(String, Float)]], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)
    sent.chunks = Some(labels.map(_.head._1).toArray)
  }

  // The head has one score, the label has another.  Here the two scores are interpolated
  // and the head and label are stored together in a single object with the score if the
  // object, the Dependency, has a valid absolute head.
  private def interpolateHeadsAndLabels(
      sentHeadPredictionScores: Array[Array[PredictionScore]],
      sentLabelPredictionScores: Array[Array[PredictionScore]],
      lambda: Float): Array[Array[Dependency]] = {
    assert(sentHeadPredictionScores.length == sentLabelPredictionScores.length)

    val sentDependencies = sentHeadPredictionScores.zip(sentLabelPredictionScores).zipWithIndex.map { case ((wordHeadPredictionScores, wordLabelPredictionScores), wordIndex) =>
      assert(wordHeadPredictionScores.length == wordLabelPredictionScores.length)

      // convert logits to probabilities so we can interpolate them later
      val wordHeadProbabilities = MathUtils.softmaxFloat(wordHeadPredictionScores.map(_._2))
      val wordLabelProbabilities = MathUtils.softmaxFloat(wordLabelPredictionScores.map(_._2))
      val wordDependencies = wordHeadPredictionScores.indices.toArray.flatMap { predictionIndex =>
          // These are probabilities not logits!  Zipping four arrays is excessive.  This may interpolate
          // unnecessarily because the newOpt may return None because of an invalid absolute head.
          val interpolatedScore = lambda * wordHeadProbabilities(predictionIndex) +
              (1.0f - lambda) * wordLabelProbabilities(predictionIndex)

          Dependency.newOpt(wordIndex, sentHeadPredictionScores.indices, wordHeadPredictionScores(predictionIndex)._1,
              wordLabelPredictionScores(predictionIndex)._1, interpolatedScore)
      }
      // Although the wordHeadPredictionScores are sorted, the wordLabelPredictionScores aren't.
      // (They can't both be.)  Therefore, they need to be resorted here.  Thankfully, only the
      // valid Dependencies remain.
      val sortedWordDependencies = wordDependencies.sortBy(-_.score)

      sortedWordDependencies.toArray
    }

    sentDependencies.toArray
  }

  private def assignDependencyLabelsUsingHexaTags(
    termTags: Array[Array[PredictionScore]],
    nonTermTags: Array[Array[PredictionScore]],
    sent: Sentence): Unit = {
    val verbose = false

    // bht is used just for debugging purposes here
    val (bht, deps, roots) = hexaDecoder.decode(termTags, nonTermTags, topK = 25, verbose)
    if(verbose && bht.nonEmpty) {
      println(bht)
      println(s"Dependencies (${deps.get.size}):")
      println(deps.mkString("\n"))
      println("Roots: " + roots.get.mkString(", "))
    }

    if(deps.nonEmpty && roots.nonEmpty) {
      // basic dependencies that replicate treebank annotations
      val depGraph = new DirectedGraph[String](deps.get, Some(sent.size), roots)
      sent.graphs += GraphMap.UNIVERSAL_BASIC -> depGraph

      // enhanced dependencies as defined by Manning
      val enhancedDepGraph = ToEnhancedDependencies.generateUniversalEnhancedDependencies(sent, depGraph)
      sent.graphs += GraphMap.UNIVERSAL_ENHANCED -> enhancedDepGraph

      // ideally, hybrid dependencies should contain both syntactic dependencies and semantic roles
      // however, this processor produces only syntactic dependencies
      sent.graphs += GraphMap.HYBRID_DEPENDENCIES -> enhancedDepGraph
    }
  }  
}

object BalaurProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[BalaurProcessor])
  val prefix:String = "BalaurProcessor"

  val OUTSIDE = "O"
  val EMPTY_GRAPH = GraphMap()

  val NER_TASK = "NER"
  val POS_TASK = "POS"
  val CHUNKING_TASK = "Chunking"
  val DEPS_HEAD_TASK = "Deps Head"
  val DEPS_LABEL_TASK = "Deps Label"
  val HEXA_TERM_TASK = "Hexa Term"
  val HEXA_NONTERM_TASK = "Hexa NonTerm"

  val PARSING_INTERPOLATION_LAMBDA = 0.6f
  val PARSING_TOPK = 5

  // maps a task name to a head index in the encoder
  val TASK_TO_INDEX = Map(
    NER_TASK -> 0,
    POS_TASK -> 1,
    CHUNKING_TASK -> 2,
    HEXA_TERM_TASK -> 3, 
    HEXA_NONTERM_TASK -> 4
  )

  def mkTokenizer(lang: String): Tokenizer = {
    lang match {
      case "PT" => new OpenDomainPortugueseTokenizer
      case "ES" => new OpenDomainSpanishTokenizer
      case _ => new OpenDomainEnglishTokenizer
    }
  }

  def mkLemmatizer(lang: String): Lemmatizer = {
    lang match {
      case "PT" => new PortugueseLemmatizer
      case "ES" => new SpanishLemmatizer
      case _ => new EnglishLemmatizer
    }
  }

  def getArgString (config: Config, argPath: String, defaultValue: Option[String]): String =
    if (config.hasPath(argPath)) config.getString(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  def newNumericEntityRecognizerOpt(seasonPathOpt: Option[String]): Option[NumericEntityRecognizer] = {
    seasonPathOpt.map(NumericEntityRecognizer(_))
  }
}
