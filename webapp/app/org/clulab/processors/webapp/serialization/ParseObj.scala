package org.clulab.processors.webapp.serialization

import org.clulab.processors.Document
import org.clulab.processors.Sentence

class ParseObj(doc: Document) {

  def mkParseObj(sentence: Sentence, sb: StringBuilder): Unit = {

    def getTd(text: String, right: Boolean = false): String = {
      val head = if (right) """<td align="right">""" else "<td>"
      val tail = "</td>"

      head + xml.Utility.escape(text) + tail
    }

    def getTdAtOptString(option: Option[Array[String]], n: Int): String = {
      val text =
        if (option.isEmpty) ""
        else option.get(n)

      getTd(text)
    }

    def getTdAtString(values: Array[String], n: Int): String = getTd(values(n))

    def getTdAtInt(values: Array[Int], n: Int): String = getTd(values(n).toString, true)

    def edgesToString(to: Int): String = {
      val edges = sentence.dependencies.map(_.incomingEdges(to)).getOrElse(Array.empty)

      edges.map(edge => sentence.words(edge._1) + "\u291c" + edge._2 + "\u2192" + sentence.words(to)).mkString(", ")
    }

    sentence.words.indices.foreach { i =>
      sb
          .append("<tr>")
          .append(s"""<td align="right">$i</td>""")
          .append(getTdAtString(sentence.raw, i))
          .append(getTdAtInt(sentence.startOffsets, i))
          .append(getTdAtInt(sentence.endOffsets, i))
          .append(getTdAtString(sentence.words, i))
          .append(getTdAtOptString(sentence.tags, i))
          .append(getTdAtOptString(sentence.lemmas, i))
          .append(getTdAtOptString(sentence.entities, i))
          .append(getTdAtOptString(sentence.norms, i))
          .append(getTdAtOptString(sentence.chunks, i))
          .append(getTd(edgesToString(i)))
          .append("</tr>")
    }
  }

  def mkHtml: String = {
    val header = """
      |<table style="margin-top: 0;">
      |  <tr>
      |    <th>Index</th>
      |    <th>Raw</th>
      |    <th>Start</th>
      |    <th>End</th>
      |    <th>Word</th>
      |    <th>Tags</th>
      |    <th>Lemmas</th>
      |    <th>Entities</th>
      |    <th>Norms</th>
      |    <th>Chunks</th>
      |    <th>Dependencies</th>
      |  </tr>
      |""".stripMargin
    val trailer = "</table>"
    val sb = new StringBuilder(header)

    doc.sentences.indices.foreach{ i =>
      val sentence = doc.sentences(i)
      val dependenciesHash = sentence.dependencies.map(_.equivalenceHash.toString).getOrElse("N/A")

      sb.append(s"<tr><td colspan='11' align='center'>Sentence ${i + 1}, sentence.equivalenceHash = ${sentence.equivalenceHash}, dependencies.equivalenceHash = $dependenciesHash</td></tr>")
      mkParseObj(sentence, sb)
    }
    sb.append(trailer)
    sb.toString
  }
}
