package com.github.tizuck
package adt.integration

import jsonGraphSchema.{
  Graph,
  Node,
  Nodes,
  SimpleEdge,
  SimpleGraph,
  TopLevelMultipleGraphs
}

import io.circe.{Decoder, HCursor, parser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonGraphsToADTSpec extends AnyWordSpec with Matchers {
  "A JSON file containing multiple graphs" should {
    "when empty be successfully parsed and decoded into an according ADT" in {
      val path = "src/test/resources/emptyMulti.json"

      val source = scala.io.Source.fromFile(path)
      val jsonContent =
        try {
          source.mkString
        } finally {
          source.close()
        }

      val parsedJSON = parser.parse(jsonContent)

      parsedJSON should be(Symbol("right"))

      for { p <- parsedJSON } yield {
        for {
          decoded <- p.as[TopLevelMultipleGraphs[Unit, Unit, Unit]]
        } yield {
          decoded should equal(
            TopLevelMultipleGraphs[Unit, Unit, Unit](
              List.empty[Graph[Unit, Unit, Unit]]
            )
          )
        }
      }
    }
    "when loaded from resources/completemulti.json be successfully parsed and decoded into an according ADT" in {
      val path = "src/test/resources/completemulti.json"

      val source = scala.io.Source.fromFile(path)
      val jsonContent =
        try {
          source.mkString
        } finally {
          source.close()
        }

      val parsedJSON = parser.parse(jsonContent)

      parsedJSON should be(Symbol("right"))

      sealed case class MetaData(
          userdefined: String,
          tpe: Option[String] = None
      )

      implicit val metaDataDecoder: Decoder[MetaData] = { (c: HCursor) =>
        for {
          tpe <- c.downField("type").as[Option[String]]
          userdefined <- c.downField("user-defined").as[String]
        } yield {
          MetaData(userdefined = userdefined, tpe = tpe)
        }
      }

      parsedJSON should be(Symbol("right"))

      for { p <- parsedJSON } yield {
        for {
          decoded <- p.as[TopLevelMultipleGraphs[MetaData, MetaData, MetaData]]
        } yield {
          println(decoded)
          decoded should equal(
            TopLevelMultipleGraphs(
              List(
                SimpleGraph[MetaData, MetaData, MetaData](
                  directed = true,
                  tpe = Some("graph type"),
                  label = Some("graph label"),
                  metadata = Some(MetaData("values")),
                  nodes = Nodes(
                    List(
                      Node[MetaData](
                        Some("node label(0)"),
                        Some(MetaData("values", Some("node type"))),
                        "0"
                      ),
                      Node[MetaData](
                        Some("node label(1)"),
                        Some(MetaData("values", Some("node type"))),
                        "1"
                      )
                    )
                  ),
                  edges = List(
                    SimpleEdge[MetaData](
                      "0",
                      "1",
                      None,
                      Some("edge relationship"),
                      directed = true,
                      Some("edge label"),
                      Some(MetaData("values"))
                    )
                  )
                ),
                SimpleGraph[MetaData, MetaData, MetaData](
                  directed = true,
                  tpe = Some("graph type"),
                  label = Some("graph label"),
                  metadata = Some(MetaData("values")),
                  nodes = Nodes(
                    List(
                      Node[MetaData](
                        Some("node label(0)"),
                        Some(MetaData("values")),
                        "0"
                      ),
                      Node[MetaData](
                        Some("node label(1)"),
                        Some(MetaData("values")),
                        "1"
                      )
                    )
                  ),
                  edges = List(
                    SimpleEdge[MetaData](
                      "1",
                      "0",
                      None,
                      Some("edge relationship"),
                      directed = true,
                      Some("edge label"),
                      Some(MetaData("values"))
                    )
                  )
                )
              )
            )
          )
        }
      }
    }
  }
}
