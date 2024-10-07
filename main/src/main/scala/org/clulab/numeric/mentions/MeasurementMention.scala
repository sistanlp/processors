package org.clulab.numeric.mentions

import org.clulab.numeric.{BaseUnitNormalizer, NullUnitNormalizer, NumberParser, UnitNormalizer}
import org.clulab.odin.{Attachment, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class MeasurementMention(
  labels: Seq[String],
  tokenInterval: Interval,
  sentence: Int,
  document: Document,
  keep: Boolean,
  foundBy: String,
  attachments: Set[Attachment],
  val value: Option[Seq[String]],
  val unit: Option[Seq[String]],
  val fromRange: Boolean,
  unitNormalizer: BaseUnitNormalizer = NullUnitNormalizer
) extends TextBoundMention(labels, tokenInterval, sentence, document, keep, foundBy, attachments) with Norm {
  val cachedNeNorm: String = calcNeNorm()

  override def neNorm: String = cachedNeNorm

  def calcNeNorm(): String = {
    assert(value.nonEmpty)
    assert(unit.nonEmpty)

    val numValueOpt =
        if (fromRange) Some(value.get.head)
        else NumberParser.parse(value.get)

    numValueOpt
        .map { numValue =>
          val unitNorm = unitNormalizer.norm(unit.get)

          s"$numValue $unitNorm"
        }
        .getOrElse(throw new RuntimeException(s"ERROR: could not parse the number [${value.mkString(" ")}] in the measurement ${raw.mkString(" ")}!"))
  }

  override def neLabel: String = {
    val unitClassOpt = unitNormalizer.unitClassOpt(unit.get)

    unitClassOpt
        .map { unitClass => s"MEASUREMENT-${unitClass.toUpperCase.replace(" ", "-")}" }
        .getOrElse("MEASUREMENT")
  }
}
