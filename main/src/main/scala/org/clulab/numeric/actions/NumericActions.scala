package org.clulab.numeric.actions

import org.clulab.numeric.{SeasonNormalizer, UnitNormalizer, WeekNormalizer}
import org.clulab.odin.{Actions, Mention, State}
import org.clulab.numeric.mentions._
import org.clulab.scala.WrappedArrayBuffer._

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

class NumericActions(seasonNormalizer: SeasonNormalizer, unitNormalizer: UnitNormalizer, weekNormalizer: WeekNormalizer) extends Actions {
  //
  // local actions
  //

  /** Converts a sequence of mentions to new types given the converter function */
  private def convert(mentions: Seq[Mention], converter: Mention => Mention, converterName: String): Seq[Mention] = {
    val convertedMentions = new ArrayBuffer[Mention]()
    for (m <- mentions) {
      try {
        convertedMentions += converter(m)
      } catch {
        case e: Exception =>
          // sometimes these conversions fail, mainly on broken texts
          // let's be robust here: report the error and move on
          System.err.println(s"WARNING: $converterName conversion failed! Recovering and continuing...")
          e.printStackTrace()
      }
    }
    convertedMentions
  }

  /** Converts a sequence of mentions to new types given the converter function */
  private def convertWithOneToManyConverter(mentions: Seq[Mention], converter: Mention => Seq[Mention], converterName: String): Seq[Mention] = {
    val convertedMentions = new ArrayBuffer[Mention]()
    for(m <- mentions) {
      try {
        convertedMentions ++= converter(m )
      } catch {
        case e: Exception =>
          // sometimes these conversions fail, mainly on broken texts
          // let's be robust here: report the error and move on
          System.err.println(s"WARNING: $converterName conversion failed! Recovering and continuing...")
          e.printStackTrace()
      }
    }
    convertedMentions
  }

  /** Constructs a NumberRange mention from a token pattern */
  def mkNumberRangeMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toNumberRangeMention, "toNumberRangeMention")
  }

  /** Constructs a MeasurementMention from a token pattern */
  def mkMeasurementMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toMeasurementMention(unitNormalizer), "toMeasurementMention")
  }

  /** Constructs a MeasurementMention from a token pattern */
  def mkSharedMeasurementMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convertWithOneToManyConverter(mentions, toSharedMeasurementMention(unitNormalizer), "toSharedMeasurementMention")
  }

  def mkPercentage(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toPercentageMention, "toPercentageMention")
  }

  def mkMeasurementWithRangeMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toMeasurementWithRangeMention(unitNormalizer), "toMeasurementWithRangeMention")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMention, "toDateRangeMention")
  }

  def mkDateRangeMentionWithVagueSeason(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithVagueSeason, "toDateRangeMentionWithVagueSeason")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithNumber(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithNumber, "toDateRangeMentionWithNumber")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithMonth(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithMonth, "toDateRangeMentionWithMonth")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithSinceRef(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithSinceRef, "toDateRangeMentionWithSinceRef")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithUntilRef(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithUntilRef, "toDateRangeMentionWithUntilRef")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithWeek(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithWeek(weekNormalizer), "toDateRangeMentionWithWeek")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateUnboundRangeMentionBefore(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateUnboundRangeMentionBefore, "toDateUnboundRangeMentionBefore")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateUnboundRangeMentionAfter(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateUnboundRangeMentionAfter, "toDateUnboundRangeMentionAfter")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithSeason(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithSeason(seasonNormalizer), "toDateRangeMentionWithSeason")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithSeasons(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithSeasons(seasonNormalizer), "toDateRangeMentionWithSeasons")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithSeasonSinceRef(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithSeasonSinceRef(seasonNormalizer), "toDateRangeMentionWithSeasonSinceRef")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionWithSeasonUntilRef(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionWithSeasonUntilRef(seasonNormalizer), "toDateRangeMentionWithSeasonUntilRef")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateUnboundRangeMentionWithSeasonBefore(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateUnboundRangeMentionWithSeasonBefore(seasonNormalizer), "toDateUnboundRangeMentionBefore")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateUnboundRangeMentionWithSeasonAfter(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateUnboundRangeMentionWithSeasonAfter(seasonNormalizer), "toDateUnboundRangeMentionAfter")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionVagueSeason(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionFromVagueSeason, "mkDateRangeMentionVagueSeason")
  }

  /** Constructs a DateRangeMention from a token pattern */
  def mkDateRangeMentionOneTokenYearRange(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateRangeMentionFromOneTokenYearRange, "mkDateRangeMentionOneTokenYearRange")
  }

  /** Constructs a DateMention from a token pattern */
  def mkDateMention(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMention, "toDateMention")
  }

  /** Constructs a DateMention from the yyyy-mm-dd single token */
  def mkDateMentionYyyyMmDd(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyyyMmDd, "toDateMentionYyyyMmDd")
  }

  /** Constructs a DateMention from the dd-mm-yyyy single token */
  def mkDateMentionDdMmYyyy(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionDdMmYyyy, "toDateMentionDdMmYyyy")
  }

  /** Constructs a DateMention from the mm-yyyy single token */
  def mkDateMentionMmYyyy(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionMmYyyy, "toDateMentionMmYyyy")
  }

  /** Constructs a DateMention from the yyyy-mm single token */
  def mkDateMentionYyyyMm(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyyyMm, "toDateMentionYyyyMm")
  }

  /** Constructs a DateMention from the yy-mm single token */
  def mkDateMentionYyMm(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyMm, "toDateMentionYyMm")
  }

  /** Constructs a DateMention from the yy-mm single token */
  def mkDateMentionYyMmDd(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionYyMmDd, "toDateMentionYyMmDd")
  }

  /** Constructs a DateMention from the yy-mm single token */
  def mkDateMentionHoliday(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionHoliday, "toDateMentionHoliday")
  }


  /** Constructs a DateMention from a Date and an Approx Modifier */
  def mkDateMentionWithModifierApprox(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionWithModifierApprox, "toDateMentionWithModifierApprox")
  }

  /** Constructs a DateMention from a Date and a Start Modifier */
  def mkDateMentionWithModifierStart(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionWithModifierStart, "toDateMentionWithModifierStart")
  }

  /** Constructs a DateMention from a Date and a Mid Modifier */
  def mkDateMentionWithModifierMid(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionWithModifierMid, "toDateMentionWithModifierMid")
  }

  /** Constructs a DateMention from a Date and an End Modifier */
  def mkDateMentionWithModifierEnd(mentions: Seq[Mention], state: State): Seq[Mention] = {
    convert(mentions, toDateMentionWithModifierEnd, "toDateMentionWithModifierEnd")
  }

  //
  // global actions below this points
  //

  /** Global action for the numeric grammar */
  def cleanupAction(mentions: Seq[Mention], state: State): Seq[Mention] =
    cleanupAction(mentions)

  def cleanupAction(mentions: Seq[Mention]): Seq[Mention] = {
    if(false) {
      println("mentions before cleanup:")
      for (m <- mentions) {
        println("\t" + m.text)
      }
    }

    val r1 = postprocessNumericEntities(mentions)
    val r2 = keepLongestMentions(r1)

    if(false) {
      println("mentions after cleanup:")
      for (m <- r2) {
        println("\t" + m.text)
      }
      println()
    }
    r2
  }

  /** filter out season homonyms (fall, spring) **/
  def postprocessNumericEntities(mentions: Seq[Mention]): Seq[Mention] = {

    def prevWordsMatch(words: Array[String], wordIndex: Int): Boolean = {
      val prevWords = words.slice(wordIndex - 2, wordIndex).map(_.toLowerCase)

      prevWords.exists(NumericActions.preSeasons) ||
          prevWords.containsSlice(NumericActions.inThe)
    }

    def contextWordsMatch(words: Array[String], wordIndex: Int): Boolean = {
      val window = 5
      val contextWords = words.slice(wordIndex - window, wordIndex + window).map(_.toLowerCase)

      contextWords.exists(NumericActions.seasons) ||
          contextWords.exists(NumericActions.yearPattern.matcher(_).matches)
    }

    val (seasonMentions, otherMentions) = mentions.partition(m => m.foundBy.contains("season"))
    val (springFall, otherSeasons) = seasonMentions.partition(m => m.text.equalsIgnoreCase("spring") || m.text.equalsIgnoreCase("fall"))
    val trueSeasons = springFall.filter { m =>
      m.tags.get.head.startsWith("NN") && {
        val words = m.sentenceObj.words
        val wordIndex = m.tokenInterval.start

        prevWordsMatch(words, wordIndex) || contextWordsMatch(words, wordIndex)
      }
    }
    trueSeasons ++ otherSeasons ++ otherMentions
  }

  /** Keeps a date (or date range) mention only if it is not contained in another */
  def keepLongestMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val (numerics, nonNumerics) = mentions.partition(NumericActions.isNumeric)
    val filteredNumerics = numerics.filterNot { outerNumeric =>
      numerics.exists { innerNumeric =>
        innerNumeric != outerNumeric &&
        innerNumeric.sentence == outerNumeric.sentence &&
        innerNumeric.tokenInterval.contains(outerNumeric.tokenInterval)
      }
    }

    filteredNumerics ++ nonNumerics
  }
}

object NumericActions {
  val seasons: Set[String] = Set("spring", "summer", "fall", "autumn", "winter")
  // Words that typically precede a season that might distinguish it from a similar verb
  val preSeasons: Set[String] = Set("this", "last", "every")
  // A common introduction to a season
  val inThe: Array[String] = Array("in", "the")
  // Match a 1 to 4 digit year
  val yearPattern = Pattern.compile("[0-9]{2}|[0-9]{4}")

  def isNumeric(m: Mention): Boolean = {
    m.isInstanceOf[DateMention] ||
      m.isInstanceOf[DateRangeMention] ||
      m.isInstanceOf[MeasurementMention] ||
      m.isInstanceOf[NumberRangeMention] ||
      m.isInstanceOf[PercentageMention]
  }
}
