package org.clulab.odin.impl

import org.clulab.struct.Interval
import org.clulab.processors.Document
import org.clulab.odin._
import org.clulab.odin.debugger.Debugger

object TokenPattern {
  val GlobalCapture = "--GLOBAL--"

  def compile(
      input: String,
      unit: String = "word",
      config: OdinConfig = OdinConfig.empty
  ): TokenPattern = {
    val compiler = new TokenPatternParsers(unit, config)
    compiler.compileTokenPattern(input)
  }

  // returns true if the next `Match*` instruction is a `MatchMention`
  def startsWithMatchMention(inst: Inst): Boolean = inst match {
    case i: MatchMention => true
    case i: Pass => startsWithMatchMention(i.getNext)
    case i: Split => startsWithMatchMention(i.lhs) && startsWithMatchMention(i.rhs)
    case i: SaveStart => startsWithMatchMention(i.getNext)
    case i: SaveEnd => startsWithMatchMention(i.getNext)
    case _ => false
  }

  case class Result(
      interval: Interval,
      groups: Map[String, Seq[Interval]],
      mentions: Map[String, Seq[Mention]]
  ) {
    val start = interval.start
    val end = interval.end
  }
}

class TokenPattern(val start: Inst) {
  import TokenPattern._

  private def assignIds(): Unit = {
    def assigner(id: Int, instructions: List[Inst]): Unit = {
      instructions match {
        case Nil => ()
        case Done :: tail =>
          // skip Done instruction
          assigner(id, tail)
        case (head: Split) :: tail if head.getPosId == 0 =>
          // only if posId hasn't been set
          head.setPosId(id)
          assigner(id + 1, head.lhs :: head.rhs :: tail)
        case (head: MatchLookAhead) :: tail if head.getPosId ==  0 =>
          head.setPosId(id)
          assigner(id + 1, head.start :: head.getNext :: tail)
        case (head: MatchLookBehind) :: tail if head.getPosId == 0 =>
          head.setPosId(id)
          assigner(id + 1, head.start :: head.getNext :: tail)
        case head :: tail if head.getPosId == 0 =>
          // only if posId hasn't been set
          head.setPosId(id)
          assigner(id + 1, head.getNext :: tail)
        case head :: tail =>
          // skip if posId has been set already
          assigner(id, tail)
      }
    }
    // don't start from zero
    assigner(1, List(start))
  }

  // assigns ids to instructions based on their position in the pattern
  // this is needed to distinguish situations like
  // x{2,4}? which gets compiled into  x x x? x? because the last two are indistinguishable with case class equality
  assignIds()

  // We want to advance token by token when the first match in the pattern is a MatchMention.
  // This allows us to find overlapping mentions that start in different tokens but end in the same one.
  // e.g. [ASPP1 and [ASPP2 are phosphorylated]] in response to EGFR
  val cautiousAdvance = startsWithMatchMention(start)

  def findPrefixOf(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    ThompsonVM.evaluate(start, tok, sent, doc, state) map {
      case (groups, mentions) =>
        // there must be one GlobalCapture only
        val globalCapture = groups(GlobalCapture).head
        Result(globalCapture, groups - GlobalCapture, mentions)
    }
  }

  def findFirstIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    val n = doc.sentences(sent).size 

    @annotation.tailrec
    def loop(i: Int): Seq[Result] = {
      if (i < n) {
        val r = Debugger.debugStart(i) {
          findPrefixOf(i, sent, doc, state)
        }
        if (r.nonEmpty) r
        else loop(i + 1)
      }
      else Nil
    }

    loop(tok)
  }

  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    @annotation.tailrec
    def collect(i: Int, collected: Seq[Result]): Seq[Result] = {
      findFirstIn(i, sent, doc, state) match {
        case Nil => collected
        case results =>
          val r = results minBy (_.interval.size)
          collect(r.end, collected ++ results)
      }
    }
    if (cautiousAdvance) {
      // move forward one token at a time
      val n = doc.sentences(sent).size
      for {
        i <- tok until n // the rest of the tokens
        r <- findPrefixOf(i, sent, doc, state)
      } yield r
    } else {
      // move forward one match at a time
      collect(tok, Nil)
    }
  }

  def findPrefixOf(sent: Int, doc: Document, state: State = new State): Seq[Result] =
    findPrefixOf(0, sent, doc, state)

  def findFirstIn(sent: Int, doc: Document, state: State = new State): Seq[Result] =
    findFirstIn(0, sent, doc, state)

  def findAllIn(sent: Int, doc: Document, state: State = new State): Seq[Result] =
    findAllIn(0, sent, doc, state)

}
