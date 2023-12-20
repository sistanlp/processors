package org.clulab.odin.debugger
import org.clulab.odin.ExtractorEngine
import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.{StringUtils, Timer}

trait DebuggerTrait {
  def activate(): Unit
  def deactivate(): Unit
  def showTrace(): Unit
  def showDeepest(): Unit
  // TODO break or pause
}

class Debugger protected () extends DebuggerTrait {
  protected var active: Boolean = true // TODO: You can turn off debugging with this!
  protected var stack: Debugger.Stack = List()
  protected var maxDepth = 0
  protected var maxStack: Debugger.Stack = stack

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

  def debugDoc[ResultType, StackFrameType <: StackFrame](extractorEngine: ExtractorEngine, doc: Document)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 0
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
      val docString = StringUtils.afterLast(doc.toString, '.')
      val textString = doc.text.getOrElse(doc.sentences.map { sentence => sentence.words.mkString(" ") }.mkString(" "))
      val message = s"""${tabs}${side} $where(doc = $docString("$textString"))"""

      message
    }

    if (active) {
      println(mkMessage("beg"))
      val result = debug(stackFrame)(block)
      println(mkMessage("end"))
      result
    }
    else {
      block
    }
  }

  def debugLoop[ResultType, StackFrameType <: StackFrame](loop: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 1
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
          .replace(' ', '.')
      val loopString = loop.toString
      val message = s"""${tabs}${side} $where(loop = $loopString)"""

      message
    }

    if (active) {
      println(mkMessage("beg"))
      val result = debug(stackFrame)(block)
      println(mkMessage("end"))
      result
    }
    else {
      block
    }
  }

  def debugExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // TODO: This could keep track of the mentions returned from the block and
    // do something special if there are none.

    // TODO: This could be part of a stack frame, no longer generic, like the TokenExtractorFrame.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 2
      val extractorString = s"""["${extractor.name}"]"""
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
      val message = s"""${tabs}${side} $where()"""

      message
    }

    if (active) {
      println(mkMessage("beg"))
      val result = debug(stackFrame)(block)
      println(mkMessage("end"))
      result
    }
    else {
      block
    }
  }

  def debugSentence[ResultType, StackFrameType <: StackFrame](index: Int, sentence: Sentence)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 3
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val sentenceString = sentence.words.mkString(" ")
      val message = s"""${tabs}${side} $where(index = $index, sentence = "$sentenceString")"""

      message
    }

    if (active) {
      println(mkMessage("beg"))
      val result = debug(stackFrame)(block)
      println(mkMessage("end"))
      result
    }
    else {
      block
    }
  }

  def debugTokInst[ResultType, StackFrameType <: StackFrame](tok: Int, inst: Inst)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 4
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val instString = inst.toString
      val message = s"""${tabs}${side} $where(tok = $tok, inst = $instString)"""

      message
    }

    if (active) {
      println(mkMessage("beg"))
      val result = debug(stackFrame)(block)
      println(mkMessage("end"))
      result
    }
    else {
      block
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

  def debugTokInst[ResultType](tok: Int, inst: Inst)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugTokInst(tok, inst)(stackFrame)(block)
  }

  def showTrace(): Unit = {
    instance.showTrace()
  }

  def showDeepest(): Unit = {
    instance.showDeepest()
  }
}
