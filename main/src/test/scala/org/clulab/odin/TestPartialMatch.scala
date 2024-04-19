package org.clulab.odin

import org.clulab.odin.impl.ThompsonVM.{Evaluator, NamedGroups, NamedMentions}
import org.clulab.odin.impl._
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.Test

class TestPartialMatch extends Test {
  @annotation.tailrec
  final def mkStarts(words: Array[String], offsets: Array[Int], index: Int = 0, offset: Int = 0): Array[Int] = {
    if (index >= words.length)
      offsets
    else {
      offsets(index) = offset
      mkStarts(words, offsets, index + 1, offset + words(index).length + 1)
    }
  }

  @annotation.tailrec
  final def mkEnds(words: Array[String], offsets: Array[Int], index: Int = 0, offset: Int = 0): Array[Int] = {
    if (index >= words.length)
      offsets
    else {
      offsets(index) = offset + words(index).length
      mkEnds(words, offsets, index + 1, offset + words(index).length + 1)
    }
  }

  def newSentence(spacedWords: String): Sentence = {
    val raw = spacedWords.split(' ')
    val length = raw.length
    val words = raw
    val startOffsets = mkStarts(words, new Array[Int](length))
    val endOffsets = mkEnds(words, new Array[Int](length))

    new Sentence(raw, startOffsets, endOffsets, words)
  }

  object TestThompsonVM {
    def evaluate(
        start: Inst,
        tok: Int,
        sent: Int,
        doc: Document,
        state: State
    ): Option[ThompsonVM.Thread]  = { // Seq[(NamedGroups, NamedMentions)] = {
      val evaluator = Evaluator(start, tok, sent, doc, state)
      // evaluate pattern and return results
      val result = evaluator.eval(tok, start) // .map(_.results).getOrElse(Nil)

      result
    }
  }

  behavior of "PartialMatch"

  it should "do something" in {
    val doc: Document = Document(Array(
      newSentence("This") // is a test .")
    ))
    val start: Inst = {
      val start = MatchToken(new WordConstraint(new ExactStringMatcher("This")))
      start.setNext(Done)

      start
    }
    val tok = 0
    val sent = 0
    val state = new State()
    val threadOpt = TestThompsonVM.evaluate(start, tok, sent, doc, state)

    threadOpt should not be empty
    threadOpt.foreach { thread =>
      thread shouldBe a [ThompsonVM.SingleThread]
      val singleThread = thread.asInstanceOf[ThompsonVM.SingleThread]

      // TODO: This should _not_ be empty
      singleThread.partialMatches shouldBe empty
    }
  }
}
