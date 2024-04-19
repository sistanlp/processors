package org.clulab.odin

import org.clulab.odin
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{Extractor, RuleReader}
import org.clulab.processors.Document

import java.io._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import scala.io.{Codec, Source}
import scala.reflect.ClassTag
import scala.util.Using

class ExtractorEngine(val extractors: Vector[Extractor], val globalAction: Action) {

  // minimum number of iterations required to satisfy the priorities
  // of all extractors
  val minIterations = extractors.map(_.priority.minIterations).max

  /** Extract mentions from a document.
   *
   *  @param doc a processor's document
   *  @return a sequence of mentions extracted from the document
   */
  def extractFrom(doc: Document): Seq[Mention] = Debugger.debugDoc(this, doc) {
    extractFrom(doc, new State)
  }

  /** Extract mentions from a document.
   *
   *  @param document a processor's document
   *  @param initialState a state instance that may be prepopulated
   *  @return a sequence of mentions extracted from the document
   */
  def extractFrom(document: Document, initialState: State): Seq[Mention] = Debugger.debug {
    @annotation.tailrec
    def loop(i: Int, state: State): Seq[Mention] = extract(i, state) match {
      case Nil if i >= minIterations => state.allMentions // we are done
      case Nil => loop(i + 1, state)
      case mentions => loop(i + 1, state.updated(mentions))
    }

    def extract(i: Int, state: State): Seq[Mention] = Debugger.debugLoop(i) {
      // extract mentions using extractors (each extractor applies its own action)
      val extractedMentions = for {
        extractor <- extractors
        if extractor.priority matches i
        mention <- extractor.findAllIn(document, state)
      } yield mention
      // apply globalAction and filter resulting mentions
      val finalMentions = for {
        mention <- globalAction(extractedMentions, state)
        if mention.isValid && !state.contains(mention)
      } yield mention
      // return the final mentions
      finalMentions
    }

    loop(1, initialState)
  }

  def extractByType[M <: Mention : ClassTag](
      document: Document,
      initialState: State = new State
  ): Seq[M] =
    extractFrom(document, initialState) flatMap {
      case m: M => Some(m)
      case _ => None
    }

}

object ExtractorEngine {

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param actions an object that contains all the actions used by the rules
   *  @param globalAction an action that will be applied to the extracted
   *                      mentions at the end of each iteration
   *  @param charset encoding to use for reading files
   *  @param ruleDir base directory to use if rules should be loaded from filesystem
   */
  def apply(
      rules: String,
      actions: Actions = new Actions,
      globalAction: odin.Action = identityAction,
      charset: Charset = UTF_8,
      ruleDir: Option[File] = None
  ): ExtractorEngine = {
    val reader = new RuleReader(actions, charset, ruleDir)
    val extractors = reader.read(rules)
    new ExtractorEngine(extractors, globalAction)
  }

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   */
  def fromRules(rules: String): ExtractorEngine = {
    apply(rules)
  }

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param charset encoding to use for reading files
   */
  def fromRules(rules: String, charset: Charset): ExtractorEngine = {
    apply(rules, charset = charset)
  }

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param actions an object that contains all the actions used by the rules
   */
  def fromRules(rules: String, actions: Actions): ExtractorEngine = {
    apply(rules, actions = actions)
  }

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param actions an object that contains all the actions used by the rules
   *  @param charset encoding to use for reading files
   */
  def fromRules(rules: String, actions: Actions, charset: Charset): ExtractorEngine = {
    apply(rules, actions = actions, charset = charset)
  }

  /** Create a new ExtractorEngine.
   *
   *  @param rules a yaml formatted string with the extraction rules
   *  @param actions an object that contains all the actions used by the rules
   *  @param globalAction an action that will be applied to the extracted
   *                      mentions at the end of each iteration
   *  @param charset encoding to use for reading files
   */
  def fromRules(rules: String, actions: Actions, globalAction: Action, charset: Charset): ExtractorEngine = {
    apply(rules, actions = actions, globalAction = globalAction, charset = charset)
  }

  private def read(file: File, charset: Charset): String = {
    implicit val codec: Codec = new Codec(charset)
    Using.resource(Source.fromFile(file)) { source =>
      val text = source.mkString
      text
    }
  }

  private def read(stream: InputStream, charset: Charset): String = {
    Using.resource (Source.fromInputStream(stream)(new Codec(charset))) { source =>
      val text = source.mkString
      text
    }
  }

  def fromFile(
      file: File,
      actions: Actions,
      globalAction: Action,
      charset: Charset
  ): ExtractorEngine = {
    apply(read(file, charset), actions = actions, globalAction = globalAction, charset = charset)
  }

  def fromStream(
      stream: InputStream,
      actions: Actions,
      globalAction: Action,
      charset: Charset
  ): ExtractorEngine = {
    apply(read(stream, charset), actions = actions, globalAction = globalAction, charset = charset)
  }

}
