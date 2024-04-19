package org.clulab.numeric

import org.clulab.numeric.actions.NumericActions
import org.clulab.odin.{ExtractorEngine, Mention}
import org.clulab.processors.Document
import org.clulab.scala.WrappedArrayBuffer._
import org.clulab.sequences.LexiconNER
import org.clulab.struct.TrueEntityValidator
import org.clulab.utils.FileUtils

import java.io.File
import scala.collection.mutable.ArrayBuffer

class NumericEntityRecognizer protected (val lexiconNer: LexiconNER, val actions: NumericActions,
    val extractor: ExtractorEngine) {

  def reloaded(ruleDir: File): NumericEntityRecognizer = {
    val extractorEngine = NumericEntityRecognizer.mkExtractor(actions, ruleDir) // Update just this part.
    new NumericEntityRecognizer(lexiconNer, actions, extractorEngine)
  }

  /** Matches the lexicon NER on this document, setting the `entities` field */
  def matchLexiconNer(document: Document): Seq[Option[Array[String]]] = {
    val originalEntities = new ArrayBuffer[Option[Array[String]]]()

    for(sent <- document.sentences) {
      originalEntities += sent.entities

      val labels = lexiconNer.find(sent)
      // this needs to happen in place, otherwise Odin does not see these labels
      // we will restore the original Sentence.entities at the end in `extractFrom`
      sent.entities = Some(labels)
    }

    originalEntities
  }

  /**
    * Entry point for numeric entity recognition
    * @param doc Input document
    * @return sets in place the sequence of NER labels and sequence of NER norms (using the TempEval-2 notation)
    */

  // TODO: KWA Skip this for now because it runs through the debugging code, confusing things.
  // Possibly make sure the debugger is off when this gets run.
  def extractFrom(doc: Document): Seq[Mention] = Seq.empty

  def extractFromOrig(doc:Document): Seq[Mention] = {
    // dictionaries
    val originalEntities = matchLexiconNer(doc)
    // grammars
    var mentions = extractor.extractFrom(doc)

    // restore the original entities
    for(i <- originalEntities.indices) {
      doc.sentences(i).entities = originalEntities(i)
    }

    // global actions *after* all grammars are done
    actions.cleanupAction(mentions)
  }
}

object NumericEntityRecognizer {
  private val rulesPath = "/org/clulab/numeric/master.yml"
  val resourceDir = {
    val cwd = new File(System.getProperty("user.dir"))
    new File(cwd, "src/main/resources")
  }
  // For the sake of SeasonNormalizer, this does have a leading /.
  val seasonPath = "/org/clulab/numeric/SEASON.tsv"
  val unitNormalizerPath = "/org/clulab/numeric/MEASUREMENT-UNIT.tsv"

  // this matches essential dictionaries such as month names
  def mkLexiconNer(seasonsPath: String): LexiconNER = {
    val kbs = Seq(
      // These shouldn't start with a leading /.
      "org/clulab/numeric/MONTH.tsv",
      "org/clulab/numeric/MEASUREMENT-UNIT.tsv",
      "org/clulab/numeric/HOLIDAY.tsv",
      if (seasonsPath.startsWith("/")) seasonsPath.drop(1) else seasonsPath
    )
    val isLocal = kbs.forall(new File(resourceDir, _).exists)
    LexiconNER(
      kbs,
      Seq(
        false, // false = case sensitive matching
        true,
        true,
        true // This should be the case for any seasonsPath.
      ),
      baseDirOpt = if (isLocal) Some(resourceDir) else None
    )
  }

  // this matches the grammars for both atomic and compositional entities
  def mkExtractor(actions: NumericActions): ExtractorEngine = {
    val rules = FileUtils.getTextFromResource(rulesPath)
    ExtractorEngine(rules, actions, actions.cleanupAction)
  }

  def mkExtractor(actions: NumericActions, ruleDir: File): ExtractorEngine = {
    val ruleFile = new File(ruleDir, rulesPath.drop(1)) // copied from RuleReader.scala
    val rules = FileUtils.getTextFromFile(ruleFile)

    ExtractorEngine(rules, actions, actions.cleanupAction, ruleDir = Some(ruleDir))
  }

  def apply(seasonPath: String = seasonPath, unitNormalizerPath: String = unitNormalizerPath): NumericEntityRecognizer = {
    val lexiconNer = mkLexiconNer(seasonPath)
    val seasonNormalizer = new SeasonNormalizer(seasonPath)
    val unitNormalizer = new UnitNormalizer(unitNormalizerPath)
    val numericActions = new NumericActions(seasonNormalizer, unitNormalizer)
    val extractorEngine = mkExtractor(numericActions)

    new NumericEntityRecognizer(lexiconNer, numericActions, extractorEngine)
  }
}