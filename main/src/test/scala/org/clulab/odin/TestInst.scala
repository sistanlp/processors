package org.clulab.odin

import org.clulab.odin.impl.{Done, ExactStringMatcher, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, RegexStringMatcher, SaveEnd, SaveStart, Split, TokenWildcard, WordConstraint}
import org.clulab.utils.Test

class TestInst extends Test {

  behavior of "Inst"

  it should "not distinguish Done and Done" in {
    val done1 = Done
    val done2 = Done

    val done1Hash = done1.##
    val done2Hash = done2.##

    done1 should be (done2)
  }

  it should "distinguish Done and Pass" in {
    val done = Done
    val pass = Pass

    val doneHash = done.##
    val passHash = pass.##

    done should not be (pass)
  }

  it should "distinguish MatchLookAhead and MatchLookBehind" in {
    val matchLookAhead = MatchLookAhead(Pass(), true)
    val matchLookBehind = MatchLookBehind(Pass(), true)

    // These will match, but == should not.
    val aheadHash = matchLookAhead.##
    val behindHash = matchLookBehind.##

    matchLookAhead should not be (matchLookBehind)
  }

  it should "distinguish SaveStart and SaveEnd" in {
    val saveStart = SaveStart("Hello", Done)
    val saveEnd = SaveEnd("Hello")

    // These will match, but == should not.
    val startHash = saveStart.##
    val endHash = saveEnd.##

    saveStart should not be (saveEnd)
  }

  it should "distinguish Done and MatchToken" in {
    val done = Done
    val matchToken = MatchToken(TokenWildcard)

    done should not be (matchToken)
  }

  it should "distinguish between Pass and Pass" in {
    val inst1 = Pass()
    val inst2 = Pass()

    val hash1a = inst1.##
    val hash2a = inst2.##

    hash1a should be (hash2a)
    inst1 should be (inst2)

    inst1.setPosId(27) // This makes hash codes different.
    inst2.setPosId(29)

    val hash1b = inst1.##
    val hash2b = inst2.##

    hash1b should not be (hash2b)
    inst1 should not be (inst2)
  }

  it should "distinguish between MatchToken and MatchToken" in {
    val inst1 = MatchToken(TokenWildcard)
    inst1.setPosId(27) // This makes hash codes different.
    val hash1 = inst1.##

    val inst2 = MatchToken(TokenWildcard)
    inst2.setPosId(29)
    val hash2 = inst2.##

    hash1 should not be (hash2)
    inst1 should not be (inst2)
  }

  //  Inst - trait
  //  Done - this
  //  Pass - copy
  //  Split - deep
  //  SaveStart - copy
  //  SaveEnd - copy
  //  MatchToken - copy
  //  MatchMention - copy
  //  MatchSentenceStart - copy
  //  MatchSentenceEnd - copy
  //  MatchLookAhead - deep
  //  MatchLookBehind - deep

  {
    // true is for deep, false is for shallow
    val instAndDeeps = Seq(
      // (Done, false), // This won't work because it is an object!
      (Pass(), false),
      (Split(MatchSentenceStart(), MatchSentenceEnd()), true),
      (SaveStart("hello", null), false),
      (SaveEnd("goodbye"), false),
      (MatchToken(TokenWildcard), false),
      (MatchMention(new ExactStringMatcher("string"), None, None), false),
      (MatchSentenceStart(), false),
      (MatchSentenceEnd(), false),
      (MatchLookAhead(Pass(), true), true),
      (MatchLookBehind(Pass(), true), true)
    )

    instAndDeeps.foreach { case (inst, deep) =>
      it should s"copy ${inst.getClass.getSimpleName} appropriately" in {
        {
          val duplicate = inst.dup()

          duplicate.getNext should be (null)
        }
        {
          val deepcopy = inst.deepcopy()

          deepcopy.getNext should be (null)
        }
        {
          inst.setNext(Pass())
          val duplicate = inst.dup()

          duplicate.getNext should be (null)
        }
        {
          val deepcopy = inst.deepcopy()

          deepcopy.getNext should not be (null)
        }
      }
    }
  }

  it should "distinguish complicated instances" in {
    val inst0 = {
      val inst = MatchToken(TokenWildcard)
      inst.setNext {
        val inst = Split(
          {
            val inst = MatchToken(TokenWildcard)
            inst.setNext {
              val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
              inst.setNext(Done)
              inst
            }
            inst
          },
          {
            val inst = new Pass
            inst.setNext {
              val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
              inst.setNext(Done)
              inst
            }
            inst
          }
        )
        inst
      }
      inst
    }
    val inst1 = {
      val inst = MatchToken(TokenWildcard)
      inst.setNext {
        val inst = MatchToken(new WordConstraint(new RegexStringMatcher("Accepted|^FAO$".r)))
        inst.setNext(Done)
        inst
      }
      inst
    }

    // These are the same.
    val inst0Hash = inst0.##
    val inst1Hash = inst1.##

    // These are different.
    val inst0HashNext = inst0.getNext.##
    val inst1HashNext = inst1.getNext.##

    val equiv = inst0 == inst1 // true
    val equivNext = inst0.getNext == inst1.getNext // false
    inst0 should be (inst1) // We are not comparing recursively.
  }

  case class CaseClass(value: Int) {
    var other: Int = 0
  }

  behavior of "CaseClasses"

  it should "compare as intended" in {
    val inst1 = CaseClass(42)
    val inst2 = CaseClass(42)
    inst2.other = inst1.other + 1

    inst1 should be (inst2)
  }

  behavior of "tuple hashes"

  it should "be called recursively" in {

    case class MyTuple(one: String, two: String) {
      var hashed = false
      override def hashCode: Int = {
        hashed = true
        5
      }
    }

    val myTuple = MyTuple("one", "two")
    val tuple = ("one", "two", myTuple).##

    myTuple.hashed should be (true)
  }
}
