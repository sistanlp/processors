package org.clulab

import org.clulab.numeric.mentions.DateMention
import org.clulab.odin.{EventMention, Mention}
import org.clulab.processors.Document

package object numeric {
  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", "))
      s.tags foreach (x => println("Tags: " + x.mkString(", ")))
      s.entities foreach (x => println("Entities: " + x.mkString(", ")))
      s.norms foreach (x => println("Norms: " + x.mkString(", ")))
      println

      val sortedMentions = mentionsBySentence(i).sortBy(_.label)
      sortedMentions foreach displayMention
      println
    }
  }

  def displayMention(mention: Mention) {
    val boundary = s"\t${"-" * 30}"
    println(s"${mention.labels} => ${mention.text}")
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(boundary)
    if (mention.arguments.nonEmpty) {
      println("\tArgs:")
      mention match {
        case em: EventMention =>
          println(s"\ttrigger: ${em.trigger}")
          displayArguments(em)
        case _ =>
          displayArguments(mention)
      }
      println(s"$boundary")
    }
    println()
  }

  def displayArguments(b: Mention): Unit = {
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          println(s"\t  * $argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}")
        }
    }
  }

  /**
    * Sets the entities and norms fields in each Sentence based on the given numeric mentions
    * @param doc This document is modified in place
    * @param mentions The numeric mentions previously extracted
    */
  def setLabelsAndNorms(doc: Document, mentions: Seq[Mention]): Unit = {
    //
    // initialize entities and norms
    //
    for(s <- doc.sentences) {
      if(s.entities.isEmpty) {
        s.entities = Some(new Array[String](s.size))
        for(i <- s.entities.get.indices) {
          s.entities.get(i) = "O"
        }
      }
      if(s.norms.isEmpty) {
        s.norms = Some(new Array[String](s.size))
        for(i <- s.norms.get.indices) {
          s.norms.get(i) = ""
        }
      }
    }

    //
    // convert numeric entities to entity labels and norms
    //
    for(mention <- mentions) {
      mention match {
        case m: DateMention =>
          val s = m.sentenceObj
          val tokenInt = m.tokenInterval
          var first = true
          for(i <- tokenInt.indices) {
            val prefix = if(first) "B-" else "I-"
            s.entities.get(i) = prefix + m.neLabel
            s.norms.get(i) = m.neNorm
            first = false
          }

        // TODO: add measurement units here

        case _ =>
          // nothing to do for other mention types
      }
    }
  }

}