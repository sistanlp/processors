package org.clulab.odinstarter

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.debugger.Debugger.visualize
import org.clulab.odin.{Actions, ExtractorEngine, Mention, identityAction}
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, GraphPattern, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, RuleReader, SaveEnd, SaveStart, Split, TokenExtractor, TokenPattern}
import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.utils.FileUtils

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8

object OdinStarter extends App {
  // When using an IDE rather than sbt, make sure the working directory for the run
  // configuration is the subproject directory so that this resourceDir is accessible.
  val resourceDir: File = new File("./src/main/resources")
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
  val (rules, ruleDirOpt) = {
    val masterResource = "/org/clulab/odinstarter/main.yml"
    // We usually want to reload rules during development,
    // so we try to load them from the filesystem first, then jar.
    // The resource must start with /, but the file probably shouldn't.
    val masterFile = new File(resourceDir, masterResource.drop(1))

    if (masterFile.exists) {
      // Read rules from file in filesystem.
      val rules = FileUtils.getTextFromFile(masterFile)
      val ruleDirOpt = Some(resourceDir)

      (rules, ruleDirOpt)
    }
    else {
      // Read rules from resource in jar.
      val rules = FileUtils.getTextFromResource(masterResource)
      val ruleDirOpt = None

      (rules, ruleDirOpt)
    }
  }
  val reader = new RuleReader(new Actions, UTF_8, ruleDirOpt)
  val extractors = reader.read(rules)
  val extractorEngine = new ExtractorEngine(extractors, identityAction)
  val document = processor.annotate("John eats cake and Jane eats pain au chocolat. Nick has A A batteries")
  //val document = processor.annotate("B A A C A A A D A A A A E")
  val mentions = extractorEngine.extractFrom(document).sortBy(_.arguments.size)

  val sentence = document.sentences.head.words.mkString(" ")  // mentions.head.sentenceObj.words.mkString(" ")


  for (mention <- mentions)
    printMention(mention)

  print("Rule view of the visualization:")
  for (extractor <- extractors)
    visualize(extractor, sentence)

  // Find all matching MatchTokens and say what they matched.

  def debugMatchTokens(): Unit = {

    val transcript = Debugger.instance.transcript

    val nameMatchTokenTokenSeq = transcript

      .filter { debuggerRecord =>

        debuggerRecord.matches && debuggerRecord.inst.isInstanceOf[MatchToken]

      }

      .map { debuggerRecord =>

        (debuggerRecord.extractor.name, debuggerRecord.inst.asInstanceOf[MatchToken], debuggerRecord.sentence.words(debuggerRecord.tok))

      }


    nameMatchTokenTokenSeq.foreach(println)

  }

  def debugDoneRules(): Unit = {

    val transcript = Debugger.instance.transcript

    val nameSentencestartTokSeq = transcript

      .filter { debuggerRecord =>

        debuggerRecord.matches && debuggerRecord.inst.isInstanceOf[Done.type]

      }

      .map { debuggerRecord =>

        (debuggerRecord.extractor.name, debuggerRecord.sentence, debuggerRecord.start, debuggerRecord.tok)

      }


    nameSentencestartTokSeq.foreach { case (name, sentence, start, tok) =>

      val words = sentence.words.clone

      words(start) = "[" + words(start)

      words(tok - 1) = words(tok - 1) + "]"


      println(s"""Rule $name matched the sentence "${words.mkString(" ")}".""")

    }

  }


  def hasWidth(inst: Inst): Boolean = {

    inst.isInstanceOf[MatchToken] || inst.isInstanceOf[MatchMention] // What is the width of this?

  }


  def debugPartialMatches(): Unit = {

    val transcript = Debugger.instance.transcript

    val keyAndMinPosMaxPos = transcript

      .groupBy { debuggerRecord =>

        (debuggerRecord.document, debuggerRecord.loop, debuggerRecord.extractor, debuggerRecord.sentenceIndex, debuggerRecord.start)

      }

      .filter { case (key, debuggerRecords) =>

        // We need to have something not SaveStart that matched and nothing that is Done.

        // Some matches are zero-width and should be ignored.  Record this fact in the Inst.

        // Alternatively, highlight empty string somewhere.

        debuggerRecords.exists { debuggerRecord => !debuggerRecord.inst.isInstanceOf[SaveStart] && debuggerRecord.matches } &&  !debuggerRecords.exists { debuggerRecord => debuggerRecord.inst.isInstanceOf[Done.type] && debuggerRecord.matches }

      }

      .map { case (key, debuggerRecords) =>

        val matchingToks = debuggerRecords.filter { debuggerRecord => debuggerRecord.matches && hasWidth(debuggerRecord.inst) }.map(_.tok)

        val minPos = key._5

        val maxPos = if (matchingToks.isEmpty) minPos else matchingToks.max + 1

        key -> (minPos, maxPos)

      }


    keyAndMinPosMaxPos.foreach { case ((document, loop, extractor, sentenceIndex, start), (minPos, maxPos)) =>

      val words = document.sentences(sentenceIndex).words.clone

      if (maxPos > minPos) {

        words(minPos) = ">" + words(minPos)

        words(maxPos - 1) = words(maxPos - 1) + "<"

      }

      else

        words(minPos) = "><" + words(minPos)


      println(s"""Rule: ${extractor.name} partially matched the sentence "${words.mkString(" ")}".""")

    }

  }

  println("\nSentence View:")
  //debugMatchTokens()

  debugDoneRules()

  debugPartialMatches()


  def visualize(extractor: Extractor, sentence: String): Unit = {

    extractor match {

      case tokenExtractor: TokenExtractor =>

        println(s"\nRule: ${tokenExtractor.name}")
        println("Visualization:")
        visualizeExtractor(tokenExtractor.pattern.start, tokenExtractor.name, sentence, 0)

      case graphExtractor: GraphExtractor => //println("\nThere was a graph extractor.")

      case crossSentenceExtractor: CrossSentenceExtractor =>

        visualizeExtractor(crossSentenceExtractor.anchorPattern.pattern.start, s"${crossSentenceExtractor.name} (Anchor)", sentence, 0)

        visualizeExtractor(crossSentenceExtractor.neighborPattern.pattern.start, s"${crossSentenceExtractor.name} (Neighbor)", sentence, 0)

      case _ => println("Unknown extractor type")

    }

  }


  private def visualizeExtractor(inst: Inst, name: String, sentence: String, indent: Int): Unit = {


    def loopsOrDeadEnds(nextInst: Inst): Boolean = {

      nextInst == null || (nextInst.getPosId <= inst.getPosId && nextInst.getPosId != 0)

    }


    val visualization = inst.visualize(sentence)


    inst match {

      case split: Split =>

        println(split.visualize(sentence))

        if (!loopsOrDeadEnds(split.lhs)) {

          print(" " * (indent + 3) + "(LHS)")

          visualizeExtractor(split.lhs, s"$name (LHS)", sentence, indent + 3)

        }

        if (!loopsOrDeadEnds(split.rhs)) {

          print(" " * (indent + 3) + "(RHS)")

          visualizeExtractor(split.rhs, s"$name (RHS)", sentence, indent + 3)

        }


      case saveStart: SaveStart =>

        println(" " * indent + saveStart.visualize(sentence))


      case saveEnd: SaveEnd =>

        println(" " * indent + saveEnd.visualize(sentence))


      case matchToken: MatchToken =>

        println(" " * indent + matchToken.visualize(sentence))


      case matchMention: MatchMention =>

        println(" " * indent + matchMention.visualize(sentence))


      case sentenceStart: MatchSentenceStart =>

        println(" " * indent + sentenceStart.visualize(sentence))


      case sentenceEnd: MatchSentenceEnd =>

        println(" " * indent + sentenceEnd.visualize(sentence))


      case pass: Pass =>

        println(" " * indent + pass.visualize(sentence))


      case Done => println(" " * indent + Done.visualize(sentence))


      case lookAhead: MatchLookAhead =>
        println(" " * indent + lookAhead.visualize(sentence))

        if (!loopsOrDeadEnds(lookAhead.start))

          visualizeExtractor(lookAhead.start, s"$name (Start)", sentence, indent)


      case lookBehind: MatchLookBehind =>
        println(" " * indent + lookBehind.visualize(sentence))

        if (!loopsOrDeadEnds(lookBehind.start))

          visualizeExtractor(lookBehind.start, s"$name (Start)", sentence, indent)


      case _ =>

    }

    if (!loopsOrDeadEnds(inst.getNext))

      visualizeExtractor(inst.getNext, s"$name (Next)", sentence, indent)

  }

  def printMention(mention: Mention, nameOpt: Option[String] = None, depth: Int = 0): Unit = {
    val indent = "    " * depth
    val name = nameOpt.getOrElse("<none>")
    val labels = mention.labels
    val words = mention.sentenceObj.words
    val tokens = mention.tokenInterval.map(mention.sentenceObj.words)

    println(indent + "     Name: " + name)
    println(indent + "   Labels: " + labels.mkString(" "))
    println(indent + " Sentence: " +  words.mkString(" "))
    println(indent + "   Tokens: " + tokens.mkString(" "))
    if (mention.arguments.nonEmpty) {
      println(indent + "Arguments:")
      for ((name, mentions) <- mention.arguments; mention <- mentions)
        printMention(mention, Some(name), depth + 1)
    }
    println()
  }
}