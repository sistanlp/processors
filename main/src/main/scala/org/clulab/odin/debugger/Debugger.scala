package org.clulab.odin.debugger

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, TokenExtractor}
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.{StringUtils, Timer}

import scala.collection.mutable.Buffer

case class DebuggerRecord(
  document: Document,
  loop: Int,
  extractor: Extractor,
  sentenceIndex: Int,
  sentence: Sentence,
  start: Int,
  tok: Int,
  inst: Inst,
  matches: Boolean
)

// TODO: This needs to be made thread-safe!
// Each Odin instance could have its own, for example.
class DebuggerContext(
  protected var depth: Int = 0,
  protected var documentOpt: Option[Document] = None,
  protected var loopOpt: Option[Int] = None,
  protected var extractorOpt: Option[Extractor] = None,
  protected var sentenceIndexOpt: Option[Int] = None,
  protected var sentenceOpt: Option[Sentence] = None,
  protected var startOpt: Option[Int] = None,
  protected var toks: List[Int] = List.empty,
  protected var insts: List[Inst] = List.empty
) {
  def isComplete: Boolean = {
    documentOpt.isDefined &&
    loopOpt.isDefined &&
    extractorOpt.isDefined &&
    sentenceIndexOpt.isDefined &&
    sentenceOpt.isDefined &&
    startOpt.isDefined &&
    toks.nonEmpty &&
    insts.nonEmpty
  }

  def getDepth: Int = depth

  def setDocument(document: Document): Unit = {
    assert(documentOpt.isEmpty)
    documentOpt = Some(document)
    depth += 1
  }

  def getDocumentOpt: Option[Document] = documentOpt

  def resetDocument(): Unit = {
    documentOpt = None
    depth -= 1
  }

  def setLoop(loop: Int): Unit = {
    assert(documentOpt.nonEmpty)
    assert(loopOpt.isEmpty)
    loopOpt = Some(loop)
    depth += 1
  }

  def getLoopOpt: Option[Int] = loopOpt

  def resetLoop(): Unit = {
    loopOpt = None
    depth -= 1
  }

  def setExtractor(extractor: Extractor): Unit = {
    assert(loopOpt.nonEmpty)
    assert(extractorOpt.isEmpty)
    extractorOpt = Some(extractor)
    depth += 1
  }

  def getExtractorOpt: Option[Extractor] = extractorOpt

  def resetExtractor(): Unit = {
    extractorOpt = None
    depth -= 1
  }

  def setSentence(sentenceIndex: Int, sentence: Sentence): Unit = {
    assert(loopOpt.nonEmpty)
    assert(sentenceIndexOpt.isEmpty)
    assert(sentenceOpt.isEmpty)
    sentenceIndexOpt = Some(sentenceIndex)
    sentenceOpt = Some(sentence)
    depth += 1
  }

  def getSentenceOpt: (Option[Int], Option[Sentence]) = (sentenceIndexOpt, sentenceOpt)

  def resetSentence(): Unit = {
    sentenceIndexOpt = None
    sentenceOpt = None
    depth -= 1
  }

  def setStart(start: Int): Unit = {
    assert(sentenceIndexOpt.nonEmpty)
    assert(sentenceOpt.nonEmpty)
    assert(startOpt.isEmpty)
    assert(toks.isEmpty)
    assert(insts.isEmpty)
    startOpt = Some(start)
    depth += 1
  }

  def getStartOpt: Option[Int] = startOpt

  def resetStart(): Unit = {
    startOpt = None
    depth -= 1
  }

  def setTok(tok: Int): Unit = {
    // If matches and go to next token, just in case there is a pass, then still have last tok
    // can this be
    assert(startOpt.nonEmpty)
    toks = tok :: toks
//    assert(insts.isEmpty)
    depth += 1
  }

  // TODO: Need to take recursion amount into account
  // That should just be counted and put into context.

  def getTokOpt: (Option[Int]) = toks.headOption

  def getToksLength: Int = toks.length

  def resetTok(): Unit = {
    toks = toks.tail
    depth -= 1
  }

  def setInst(inst: Inst): Unit = {
    if (toks.isEmpty)
      println("How?")
//    assert(tokOpt.nonEmpty)
    // There can be multiple inst before we return.
    insts = inst :: insts
    depth += 1
  }

  def getInstOpt: Option[Inst] = insts.headOption

  def getInstsLength: Int = insts.length

  def resetInst(): Unit = {
    insts = insts.tail
    depth -= 1
  }

  def setMatches(matches: Boolean): DebuggerRecord = {
//    assert(isComplete)
    DebuggerRecord(documentOpt.get, loopOpt.get, extractorOpt.get, sentenceIndexOpt.get, sentenceOpt.get,
      startOpt.get, toks.head, insts.head, matches)
  }
}

trait DebuggerTrait {
  def activate(): Unit
  def deactivate(): Unit
  def showTrace(): Unit
  def showDeepest(): Unit
  // TODO break or pause
}

class Debugger protected () extends DebuggerTrait {
  protected var active: Boolean = true // TODO: You can turn off debugging with this!
  protected var quiet: Boolean = true // TODO: You can turn off the printing of messages with this.
  protected var stack: Debugger.Stack = List()
  protected var maxDepth = 0
  protected var maxStack: Debugger.Stack = stack
  protected val context = new DebuggerContext()
  val transcript: Buffer[DebuggerRecord] = Buffer.empty

  def activate(): Unit = active = true

  def deactivate(): Unit = active = false

  def debug[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    if (active) {
      stack = stackFrame :: stack
      if (stack.length > maxDepth)
        maxStack = stack

      val result = try {
        block
      }
      finally {
        stack = stack.tail
      }
      val execTime = stackFrame.stopTimer()
//      println(s"Execution time for stack frame: $execTime nanoseconds")
      result
    }
    else {
      block
    }
  }

  def debugWithMessage[ResultType, StackFrameType <: StackFrame](mkMessage: (String) => String)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    if (active) {
      if (!quiet) println(mkMessage("beg"))
      val result = debug(stackFrame)(block)
      if (!quiet) println(mkMessage("end"))
      result
    }
    else {
      block
    }
  }

  def debugDoc[ResultType, StackFrameType <: StackFrame](extractorEngine: ExtractorEngine, doc: Document)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
      val docString = StringUtils.afterLast(doc.toString, '.')
      val textString = doc.text.getOrElse(doc.sentences.map { sentence => sentence.words.mkString(" ") }.mkString(" "))
      val message = s"""${tabs}${side} $where(doc = $docString("$textString"))"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setDocument(doc)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetDocument()

    result
  }

  def debugLoop[ResultType, StackFrameType <: StackFrame](loop: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
          .replace(' ', '.')
      val loopString = loop.toString
      val message = s"""${tabs}${side} $where(loop = $loopString)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setLoop(loop)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetLoop()
    result
  }

  def debugExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // TODO: This could keep track of the mentions returned from the block and
    // do something special if there are none.

    // TODO: This could be part of a stack frame, no longer generic, like the TokenExtractorFrame.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val extractorString = s"""["${extractor.name}"]"""
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
      val message = s"""${tabs}${side} $where()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setExtractor(extractor)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetExtractor()
    result
  }

  def debugSentence[ResultType, StackFrameType <: StackFrame](index: Int, sentence: Sentence)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val sentenceString = sentence.words.mkString(" ")
      val message = s"""${tabs}${side} $where(index = $index, sentence = "$sentenceString")"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setSentence(index, sentence)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetSentence()
    result
  }

  def debugStart[ResultType, StackFrameType <: StackFrame](start: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val method = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
      val obj = StringUtils.afterLast(StringUtils.beforeLast(stackFrame.sourceCode.enclosing.value, '.'), '.')
      val message = s"""${tabs}${side} $obj.$method(start = $start)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setStart(start)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetStart()
    result
  }

  def debugTok[ResultType, StackFrameType <: StackFrame](tok: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val message = s"""${tabs}${side} $where(tok = $tok)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setTok(tok)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetTok()
    result
  }

  def debugInst[ResultType, StackFrameType <: StackFrame](inst: Inst)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
          .replace(' ', '.')
      val instString = inst.toString
      val message = s"""${tabs}${side} $where(inst = $instString)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setInst(inst)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetInst()
    result
  }

  def debugMatches(matches: Boolean): Unit = {

    def mkMessage(depth: Int): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
//      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
//          .replace("#", s"$extractorString.")
//          .replace(' ', '.')
//      val instString = inst.toString
//      val message = s"""${tabs}${side} $where(inst = $instString)"""
      val message = s"""${tabs}$matches"""

      message
    }

    val message = mkMessage(context.getDepth)

    if (active) {
      if (!quiet) println(message)
      transcript += context.setMatches(matches)
    }
  }

  def showTrace(stack: Debugger.Stack): Unit = {
    println("Here's your trace...")
    stack.zipWithIndex.foreach { case (stackFrame, index) =>
      println(s"$index: $stackFrame")
    }
  }

  def showTrace(): Unit = showTrace(stack)

  def showDeepest(): Unit = showTrace(maxStack)
}

object Debugger extends DebuggerTrait {
  type Stack = List[StackFrame]

  lazy val instance = new Debugger() // TODO: In the end this will not be global unless it can be thread-aware.

  override def activate(): Unit = instance.activate()

  override def deactivate(): Unit = instance.deactivate()

  def debugFrame[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: StackFrameType => ResultType): ResultType = {
    instance.debug(stackFrame)(block(stackFrame))
  }

  def debug[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    instance.debug(stackFrame)(block)
  }

  def debug[ResultType](block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debug(stackFrame)(block)
  }

  // The extractorEngine needs to come in with the doc so that client code does not need to call debug
  // methods on the extractorEngine itself.
  def debugDoc[ResultType](extractorEngine: ExtractorEngine, doc: Document)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugDoc(extractorEngine, doc)(stackFrame)(block)
  }

  def debugLoop[ResultType](loop: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugLoop(loop)(stackFrame)(block)
  }

  def debugExtractor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugExtractor(extractor)(stackFrame)(block)
  }

  // TODO: We should know the document already, so the index should suffice.
  def debugSentence[ResultType](index: Int, sentence: Sentence)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugSentence(index, sentence)(stackFrame)(block)
  }

  def debugStart[ResultType](start: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugStart(start)(stackFrame)(block)
  }

  def debugTok[ResultType](tok: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugTok(tok)(stackFrame)(block)
  }

  def debugInst[ResultType](inst: Inst)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugInst(inst)(stackFrame)(block)
  }

  def debugMatches(matches: Boolean) = instance.debugMatches(matches)

  def showTrace(): Unit = {
    instance.showTrace()
  }

  def showDeepest(): Unit = {
    instance.showDeepest()
  }

  type InstToDebuggerRecords = Map[Inst, Seq[DebuggerRecord]]

  def visualize(extractor: Extractor, sentence: Sentence): Unit = {
    val transcript = instance.transcript.toSeq.filter { debuggerRecord =>
      // Only worry about this extractor and sentence's records and only matches.
      // TODO: Wouldn't it me nice if these were sorted?
      debuggerRecord.extractor == extractor && debuggerRecord.sentence == sentence && debuggerRecord.matches
    }
    val instToDebuggerRecords: InstToDebuggerRecords = transcript.groupBy(_.inst)

    extractor match {
      case tokenExtractor: TokenExtractor =>
        println(s"\nThere was an extractor: ${tokenExtractor.name}")
        visualizeExtractor(tokenExtractor.pattern.start, "", sentence, 0, instToDebuggerRecords)
      case graphExtractor: GraphExtractor => println("\nThere was a graph extractor.")
      case crossSentenceExtractor: CrossSentenceExtractor =>
        visualizeExtractor(crossSentenceExtractor.anchorPattern.pattern.start, s"${crossSentenceExtractor.name} (Anchor)", sentence, 0, instToDebuggerRecords)
        visualizeExtractor(crossSentenceExtractor.neighborPattern.pattern.start, s"${crossSentenceExtractor.name} (Neighbor)", sentence, 0, instToDebuggerRecords)
      case _ => println("Unknown extractor type")
    }
  }

  private def visualizeExtractor(inst: Inst, name: String, sentence: Sentence, depth: Int, instToDebuggerRecords: InstToDebuggerRecords): Unit = {
    val debuggerRecords = instToDebuggerRecords.get(inst).getOrElse(Seq.empty)
    val indent = " " * (depth * 3) + name
    val sentenceString = sentence.getSentenceText

    def loopsOrDeadEnds(nextInst: Inst): Boolean = {
      nextInst == null || (nextInst.getPosId <= inst.getPosId && nextInst.getPosId != 0)
    }

    val matchVisualization = debuggerRecords.map {debuggerRecord =>
      val tok = debuggerRecord.tok
      val word = debuggerRecord.sentence.words.lift(tok).getOrElse("<EOS>")

      s"$tok: $word"
    }.mkString(", which matches(", ", ", ")")
    val visualization = indent + inst.visualize(sentenceString) + matchVisualization
    println(visualization)

    inst match {
      case split: Split =>
        if (!loopsOrDeadEnds(split.lhs))
          visualizeExtractor(split.lhs, "(LHS) ", sentence, depth + 1, instToDebuggerRecords)
        if (!loopsOrDeadEnds(split.rhs))
          visualizeExtractor(split.rhs, "(RHS) ", sentence, depth + 1, instToDebuggerRecords)

      case saveStart: SaveStart =>

      case saveEnd: SaveEnd =>

      case matchToken: MatchToken =>

      case matchMention: MatchMention =>

      case sentenceStart: MatchSentenceStart =>

      case sentenceEnd: MatchSentenceEnd =>

      case pass: Pass =>

      case Done =>

      case lookAhead: MatchLookAhead =>
        if (!loopsOrDeadEnds(lookAhead.start))
          visualizeExtractor(lookAhead.start, "(Start) ", sentence, depth + 1, instToDebuggerRecords)

      case lookBehind: MatchLookBehind =>
        if (!loopsOrDeadEnds(lookBehind.start))
          visualizeExtractor(lookBehind.start, "(Start) ", sentence, depth + 1, instToDebuggerRecords)

      case _ =>
    }
    if (!loopsOrDeadEnds(inst.getNext))
      visualizeExtractor(inst.getNext, name, sentence, depth, instToDebuggerRecords)
  }
}
