package org.clulab.odin

import org.clulab.odin.impl.{Inst, MatchLookAhead, MatchLookBehind, Split, TokenExtractor}
import org.clulab.utils.Test

import scala.annotation.tailrec

class TestPosId extends Test {

  val rules =
    """
      |taxonomy:
      |- Eating
      |
      |rules:
      |- name: test-rule
      |  priority: 1
      |  label: Eating
      |  type: token
      |  pattern: |
      |    (?<= hello hello) Let them hello hello eat cake
      |
    """.stripMargin

  behavior of "Inst"

  def getChildren(inst: Inst): List[Inst] = {
    val stepChildren = inst match {
      case split: Split => split.lhs :: split.rhs :: Nil
      case matchLook: MatchLookAhead => matchLook.start :: Nil
      case matchLook: MatchLookBehind => matchLook.start :: Nil
      case _: Inst => Nil
    }
    val children = stepChildren :+ inst.next

    children
  }

  def collectInsts(inst: Inst): List[Inst] = {

    @tailrec
    def loop(ins: List[Inst], outs: List[Inst]): List[Inst] = {
      ins match {
        case Nil => outs
        case null :: tail => loop(tail, outs)
        case head :: tail =>
          val refMatchOpt = outs.find { inst => head.eq(inst) }

          if (refMatchOpt.isEmpty) {
            val newIns = getChildren(head) ++ tail
            val newOuts = head :: outs

            loop(newIns, newOuts)
          }
          else
            loop(tail, outs)
      }
    }

    loop(inst :: Nil, Nil).reverse
  }

  it should "have a different posId for each instance" in {
    val ee = ExtractorEngine(rules)
    val start = ee.extractors.head.asInstanceOf[TokenExtractor].pattern.start
    val insts = collectInsts(start)
    val posIds = insts.map(_.posId)
    val distinctPosIds = posIds.distinct

    insts.foreach(println)

    // Done's posId should be zero along with anything
    // that was missed when assigned IDs.
    distinctPosIds.length should be (posIds.length)
  }
}
