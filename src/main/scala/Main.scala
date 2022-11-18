package com.github.tizuck

import com.github.tizuck.jsonGraphSchema.TopLevelSingleGraph
import io.circe.parser

object Main extends App {
  val json =
    """
      |{
      |      "graph":{
      |        "metadata":{"meta":0},
      |        "nodes":{
      |          "q1":{
      |            "label":"1"
      |          }
      |        },
      |        "edges":[
      |          {
      |            "source":"q1",
      |            "target":"q2",
      |           "label":"foo"
      |          }
      |        ]
      |      }
      |    }
      |""".stripMargin

  import io.circe.generic.auto._

  sealed case class MetaData(meta:Int)

  val parsed = parser.parse(json)
  println(parsed)
  for{p <- parsed} yield {
    for{ tpe <- p.as[TopLevelSingleGraph[MetaData,Unit,Unit]]} yield {
      println(tpe)
    }
  }
}
