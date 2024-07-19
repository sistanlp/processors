package org.clulab.odin.impl

import org.clulab.processors.Document
import org.clulab.odin._
import org.clulab.struct.Interval


trait Extractor {
  def name: String
  def labels: Seq[String]
  def label: String = labels.head  // the first label in the sequence is the default
  def priority: Priority
  def keep: Boolean  // should we keep mentions generated by this extractor?
  def action: Action

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention]

  def findAllIn(doc: Document, state: State): Seq[Mention] = for {
    i <- 0 until doc.sentences.size
    m <- findAllIn(i, doc, state)
  } yield m

  protected def newTextBoundMention(interval: Interval, sent: Int, doc: Document): TextBoundMention =
    new TextBoundMention(labels, interval, sent, doc, keep, name)

  protected def newEventMention(trigger: TextBoundMention, args: Map[String, Seq[Mention]], interval: Interval, sent: Int, doc: Document): EventMention =
    new EventMention(labels, interval, trigger, args, Map.empty, sent, doc, keep, name)

  protected def newRelationMention(args: Map[String, Seq[Mention]], interval: Interval, sent: Int, doc: Document): RelationMention =
    new RelationMention(labels, interval, args, Map.empty, sent, doc, keep, name)
}

class TokenExtractor(
    val name: String,
    val labels: Seq[String],
    val priority: Priority,
    val keep: Boolean,
    val action: Action,
    val pattern: TokenPattern
) extends Extractor {

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = {
    val results = pattern.findAllIn(sent, doc, state)
    val mentions = for (r <- results) yield mkMention(r, sent, doc)
    action(mentions, state)
  }

  def mkMention(r: TokenPattern.Result, sent: Int, doc: Document): Mention = {
    val groupsTrigger = r.groups.keys find (_.equalsIgnoreCase("trigger"))
    val mentionsTrigger = r.mentions.keys find (_.equalsIgnoreCase("trigger"))
    (groupsTrigger, mentionsTrigger) match {
      case (Some(groupTriggerKey), Some(mentionTriggerKey)) =>
        // Can't have both notations
        throw new RuntimeException("Can't specify a trigger as both named capture and named mention")
      case (Some(groupTriggerKey), None) =>
        // having several triggers in the same rule is not supported
        // the first will be used and the rest ignored
        val int = r.groups(groupTriggerKey).head
        val trigger = newTextBoundMention(int, sent, doc)
        val groups = r.groups - groupTriggerKey transform { (argName, intervals) =>
          intervals.map(i => newTextBoundMention(i, sent, doc))
        }
        val args = mergeArgs(groups, r.mentions)
        newEventMention(trigger, args, r.interval, sent, doc)
      case (None, Some(mentionTriggerKey)) =>
        // having several triggers in the same rule is not supported
        // the first will be used and the rest ignored
        val origTrigger = r.mentions(mentionTriggerKey).head
        val trigger = newTextBoundMention(origTrigger.tokenInterval, sent, doc)
        val groups = r.groups transform { (argName, intervals) =>
          intervals.map(i => newTextBoundMention(i, sent, doc))
        }
        val mentions = r.mentions - mentionTriggerKey
        val args = mergeArgs(groups, mentions)
        newEventMention(trigger, args, r.interval, sent, doc)
      case (None, None) if r.groups.nonEmpty || r.mentions.nonEmpty =>
        // result has arguments and no trigger, create a RelationMention
        val groups = r.groups transform { (argName, intervals) =>
          intervals.map(i => newTextBoundMention(i, sent, doc))
        }
        val args = mergeArgs(groups, r.mentions)
        newRelationMention(args, r.interval, sent, doc)
      case (None, None) =>
        // result has no arguments, create a TextBoundMention
        newTextBoundMention(r.interval, sent, doc)
    }
  }

  type Args = Map[String, Seq[Mention]]
  def mergeArgs(m1: Args, m2: Args): Args = {
    val merged = for (name <- m1.keys ++ m2.keys) yield {
      val args = m1.getOrElse(name, Vector.empty) ++ m2.getOrElse(name, Vector.empty)
      name -> args.distinct
    }
    merged.toMap
  }
}

class GraphExtractor(
    val name: String,
    val labels: Seq[String],
    val priority: Priority,
    val keep: Boolean,
    val action: Action,
    val pattern: GraphPattern,
    val config: OdinConfig
) extends Extractor {

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = {
    val mentions = pattern.getMentions(sent, doc, state, labels, keep, name)
    action(mentions, state)
  }
}


class CrossSentenceExtractor(
  val name: String,
  val labels: Seq[String],
  val priority: Priority,
  val keep: Boolean,
  val action: Action,
  // the maximum number of sentences to look behind for pattern2
  val leftWindow: Int,
  // the maximum number of sentences to look ahead for pattern2
  val rightWindow: Int,
  val anchorPattern: TokenExtractor,
  val neighborPattern: TokenExtractor,
  val anchorRole: String,
  val neighborRole: String
) extends Extractor {

  // inspect windows
  if (leftWindow == 0 && rightWindow == 0) {
    throw OdinException(s"cross-sentence pattern for '$name' must have window > 0 either to the left or to the right")
  }

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = {

    def getMentionsWithLabel(m: Mention): Seq[Mention] = {
      state.mentionsFor(m.sentence, m.tokenInterval).filter{ mention =>
        // the span should match exactly
        (mention.tokenInterval == m.tokenInterval) &&
        // the label should match
        (mention.matches(m.label))
      }
    }

    anchorPattern.findAllIn(sent, doc, state) match {
      // the rule failed
      case Nil => Nil
      // the anchor matched something
      case anchorMentions =>

        // check for valid window values
        if (leftWindow < 0)  throw OdinException(s"left-window for '$name' must be >= 0")
        if (rightWindow < 0) throw OdinException(s"right-window for '$name' must be >= 0")

        val mentions = for {
          i <- sent - leftWindow to sent + rightWindow
          // is the sentence within the allotted window?
          if 0 <= i && i < doc.sentences.length
          // the neighbor cannot be in the same sentence as the anchor
          if i != sent
          // find the mentions in the state that match the given span and label
          anchor <- anchorMentions.flatMap(getMentionsWithLabel)
          //_ = println(s"Anchor:${anchor.labels}: '${anchor.text}' foundBy ${anchor.foundBy}")
          // attempt to match the neighbor's pattern
          neighbor <- neighborPattern.findAllIn(i, doc, state).flatMap(getMentionsWithLabel)
          //_ = println(s"Neighbor:${neighbor.labels}: '${neighbor.text}' foundBy ${neighbor.foundBy}")
          // the anchor and neighbor cannot be in the same sentence
          // if anchor.sentence != neighbor.sentence
        } yield mkMention(anchor, neighbor)

        action(mentions, state)
    }
  }

  def mkMention(anchor: Mention, neighbor: Mention): CrossSentenceMention = {
    // FIXME: we should redo Mention's interval (and sentence)
    new CrossSentenceMention(
      labels,
      anchor = anchor,
      neighbor = neighbor,
      arguments = Map(anchorRole -> Seq(anchor), neighborRole -> Seq(neighbor)),
      anchor.document,
      keep,
      name
    )
  }
}
