package org.clulab.odin.debugger

import org.clulab.odin.ExtractorEngine
import org.clulab.serialization.json.JSONSerializer
import org.clulab.utils.Test
import org.json4s.jackson.JsonMethods

class TestDebugger extends Test {
  val rule = """
    |rules:
    | - name: test
    |   type: token
    |   label: TestMention
    |   pattern: |
    |     [lemma=I] []* [lemma=dance]
    |""".stripMargin
  val odin = ExtractorEngine(rule)

  behavior of "Debugger"

  it should "show deepest stack trace" in {
    val json = """{
      |"sentences":[{
      |  "words":["I","am","going","to","dance","."],
      |  "startOffsets":[0,1,4,10,13,18],
      |  "endOffsets":[1,3,9,12,18,19],
      |  "raw":["I","'m","going","to","dance","."],
      |  "tags":["PRP","VBP","VBG","TO","VB","."],
      |  "lemmas":["I","be","go","to","dance","."],
      |  "entities":["O","O","O","O","O","O"],
      |  "norms":["O","O","O","O","O","O"],
      |  "chunks":["B-NP","B-VP","I-VP","I-VP","I-VP","O"],
      |  "graphs":{
      |    "universal-enhanced":{
      |      "edges":[
      |        {"source":2,"destination":0,"relation":"nsubj"},
      |        {"source":2,"destination":1,"relation":"aux"},
      |        {"source":2,"destination":4,"relation":"xcomp"},
      |        {"source":2,"destination":5,"relation":"punct"},
      |        {"source":4,"destination":0,"relation":"nsubj:xsubj"},
      |        {"source":4,"destination":3,"relation":"mark"}],
      |      "roots":[2]},
      |    "universal-basic":{
      |      "edges":[
      |        {"source":2,"destination":0,"relation":"nsubj"},
      |        {"source":2,"destination":1,"relation":"aux"},
      |        {"source":2,"destination":4,"relation":"xcomp"},
      |        {"source":2,"destination":5,"relation":"punct"},
      |        {"source":4,"destination":3,"relation":"mark"}],
      |      "roots":[2]}}}]}
      |""".stripMargin
    val document = JSONSerializer.toDocument(JsonMethods.parse(json))
    val mentions = odin.extractFrom(document) // mentions should not be empty

    Debugger.showDeepest() // This should turn up something.
  }
}
