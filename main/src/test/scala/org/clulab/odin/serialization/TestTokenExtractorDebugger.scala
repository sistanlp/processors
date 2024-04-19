package org.clulab.odin.serialization

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{Extractor, RuleReader}
import org.clulab.odin.{Actions, ExtractorEngine, identityAction}
import org.clulab.odinstarter.OdinStarter.resourceDir
import org.clulab.processors.{Document, Sentence}
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Test

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8

class TestTokenExtractorDebugger extends Test {
  val customLexiconNer = { // i.e., Named Entity Recognizer
    val kbsAndCaseInsensitiveMatchings: Seq[(String, Boolean)] = Seq(
      // You can add additional kbs (knowledge bases) and caseInsensitiveMatchings here.
      ("org/clulab/odinstarter/FOOD.tsv", true) // ,
      // ("org/clulab/odinstarter/RESTAURANTS.tsv", false)
    )
    val kbs = kbsAndCaseInsensitiveMatchings.map(_._1)
    val caseInsensitiveMatchings = kbsAndCaseInsensitiveMatchings.map(_._2)
    val isLocal = kbs.forall(new File(resourceDir, _).exists)
    val baseDirOpt = if (isLocal) Some(resourceDir) else None

    LexiconNER(kbs, caseInsensitiveMatchings, baseDirOpt)
  }
  val processor = new CluProcessor(optionalNER = Some(customLexiconNer))

  class Rule(val name: String, val text: String)

  object Rule {

    def apply(name: String, pattern: String): Rule = {
      val text = s"""
        |- name: $name
        |  priority: "1"
        |  label: Test
        |  type: token
        |  pattern: |
        |    $pattern
      """.stripMargin

      new Rule(name, text)
    }
  }

  def visualize(extractor: Extractor, sentence: Sentence): Unit = {
    // Consult the debugger.  Eyeball output for now?
    Debugger.visualize(extractor, sentence)
  }

  def test(rule: Rule, document: Document): Unit = {
    behavior of rule.name

    it should "debug as expected" in {
      val reader = new RuleReader(new Actions, UTF_8, None)
      val extractors = reader.read(rule.text)

      val extractorEngine = new ExtractorEngine(extractors, identityAction)
      val _ = extractorEngine.extractFrom(document)

      visualize(extractors.head, document.sentences.head)
    }
  }

  val rules = Seq(
//    Rule("foods-from-lexicon", "[entity='B-FOOD'] [entity='I-FOOD']*"),
//    Rule("person-from-lexicon", "[entity='B-PER'] [entity='I-PER']*"),
//    Rule("more-foods-from-lexicon", "(?<= hello) [entity='B-FOOD'] [entity='I-FOOD']* @theme:Person $"),
//    Rule("more-person-from-lexicon", "^ @theme:Food [entity='B-PER'] [entity='I-PER']* (?= goodbye)"),
//    Rule("a and b", "a{1, 3} | b+"),
//    Rule("verb-tense", "[entity='B-PER'] [entity='I-PER']* (?= ate|eats|eating) [entity='B-FOOD'] [entity='I-FOOD']* "),
//    Rule("optional-word-followed-by-c-and-d", "(word1 | word2)? (c|d)"),
//    Rule("food-verb-food", "[entity='B-FOOD'] [entity='I-FOOD']* (?= eats|ate|eating) [entity='B-FOOD'] [entity='I-FOOD']*"),
//    Rule("person-and-food", "[entity='B-PER'] [entity='I-PER']* (?= eats|ate|eating) [entity='B-FOOD'] [entity='I-FOOD']*"),
//    Rule("food-or-person", "([entity='B-FOOD'] [entity='I-FOOD']* | [entity='B-PER'] [entity='I-PER']*)"),
    Rule("food-followed-by-person", "[entity='B-FOOD'] [entity='I-FOOD']* (?= [entity='B-PER'])"),
//    Rule("no-person-at-start", "(?<![entity='B-PER']) [entity='B-FOOD'] [entity='I-FOOD']*"), //negative lookbehind
//    Rule("3-letter-person-name", "[entity ='B-PER']{3} [entity='B-FOOD'] [entity='I-FOOD']*"),
//    Rule("2-to-6-letter-food", "[entity='B-PER'] [entity='I-PER']* [entity='B-FOOD']{2,6}"),
//    Rule("partial-match-1", "[entity='B-PER'] [entity='I-PER']* (?= walk|walking)"),
//    Rule("4 As", "[word='A']{4}")
  )
//  val sentence = "B A A C A A A D A A A A E"
  val sentence = "pain au chocolat John"
  val document = processor.annotate(sentence)

  rules.foreach(test(_, document))
}
