package org.clulab.odin.debugger

import org.clulab.odin.impl.ThompsonVM.PartialMatch
import org.clulab.struct.Interval
import org.clulab.utils.Test


class TestThompsonVM extends Test  {
      val partialMatch1 = PartialMatch(0, 1, 2, "Rule1")
      val partialMatch2 = PartialMatch(0, 2, 3, "Rule2")
      val partialMatch3 = PartialMatch(1, 0, 1, "Rule1")

      val interval1 = Interval(1, 2)
      val interval2 = Interval(2, 3)
      val interval3 = Interval(0, 1)

      assert(partialMatch1.sentenceId == 0)
      assert(partialMatch1.startToken == 1)
      assert(partialMatch1.endToken == 2)
      assert(partialMatch1.ruleName == "Rule1")

      assert(partialMatch2.sentenceId == 0)
      assert(partialMatch2.startToken == 2)
      assert(partialMatch2.endToken == 3)
      assert(partialMatch2.ruleName == "Rule2")

      assert(partialMatch3.sentenceId == 1)
      assert(partialMatch3.startToken == 0)
      assert(partialMatch3.endToken == 1)
      assert(partialMatch3.ruleName == "Rule1")

      assert(interval1 == Interval(partialMatch1.startToken, partialMatch1.endToken))
      assert(interval2 == Interval(partialMatch2.startToken, partialMatch2.endToken))
      assert(interval3 == Interval(partialMatch3.startToken, partialMatch3.endToken))
}
