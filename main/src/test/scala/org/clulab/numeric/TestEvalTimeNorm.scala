package org.clulab.numeric
import org.clulab.processors.clu.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestEvalTimeNorm extends FlatSpec with Matchers{

  behavior of "temporal parser"

  it should "not degrade in performance" in {
    val expectedFscore = 0.81
    val proc = new CluProcessor(seasonPathOpt = Some("/org/clulab/numeric/custom/SEASON.tsv")) // there are lots of options for this
    val ner = NumericEntityRecognizer(seasonPath = "/org/clulab/numeric/custom/SEASON.tsv")
    val actualFscore = EvalTimeNorm.test(proc, ner)
    actualFscore should be >= expectedFscore
  }

}

