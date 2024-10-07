package org.clulab.numeric

import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestNumericMentions extends Test {
  val processor = new CluProcessor()

  behavior of "MeasurementMention"

  it should "not throw an exception" in {
    val text = "(c) a retention licence, fourty five days; and  ."
    val document = processor.annotate(text)

    document
  }

  behavior of "PercentageMention"

  it should "not throw an exception" in {
    val text = "pdf 75 Some of the major sectoral plans include the national energy policy, transport policy, forest and wildlife policy, national environmental sanitation strategy etc. 76 https://www.thegasconsortium.com/documents/GMP-Final-Jun16.pdf 77 http://www.energycom.gov.gh/files/Renewable-Energy-Masterplan-February-2019.pdf 78 http://energycom.gov.gh/files/SE4ALL-GHANA%20ACTION%20PLAN.pdf 79 http://www.energycom.gov.gh/files/Ghana%20Integrated%20Power%20System%20Master%20Plan%20_Volume%202.pdf Page | 80 In 2017, the Ministry of Energy started a comprehensive review of the National Energy Policy, which is still ongoing."
    val document = processor.annotate(text)

    document
  }
}
