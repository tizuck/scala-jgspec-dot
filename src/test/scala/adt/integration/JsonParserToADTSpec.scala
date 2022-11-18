package com.github.tizuck
package adt.integration

import jsonGraphSchema.{
  Node,
  Nodes,
  SimpleEdge,
  SimpleGraph,
  TopLevelSingleGraph,
  UndirectedHyperEdge,
  UndirectedHyperGraph
}

import io.circe.{Decoder, DecodingFailure, HCursor, ParsingFailure}
import io.circe.parser._
import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec

/**
 * Test for decoding JSON files given on github page of the JSON graph specification
 * ([[https://github.com/jsongraph/json-graph-specification]]) into an AST of [[jsonGraphSchema]].
 */
class JsonParserToADTSpec extends AnyWordSpec {

  val empty = "src/test/resources/empty.json"
  val singleNodesOnly = "src/test/resources/nodesonlysingle.json"
  val singleGraphNodesEdges = "src/test/resources/nodesedgessingle.json"
  val singleGraphHyper = "src/test/resources/hyperedgessingle.json"
  val completeSingle = "src/test/resources/completesingle.json"

  "A Json Parser" should {
    "parse an empty single graph and the ADT" should {
      "contain an empty graph" in {
        val expectedADT = TopLevelSingleGraph(SimpleGraph[Unit, Unit, Unit]())

        val res = compareSingleGraphADTs[Unit, Unit, Unit](expectedADT, empty)
        assert(res.isRight)
      }
    }
    "parse a single graph with nodes only and the ADT" should {
      "contain a single graph with the two expected nodes" in {
        val expectedADT = TopLevelSingleGraph(
          SimpleGraph[Unit, Unit, Unit](nodes =
            Nodes(List(Node[Unit](jsonkey = "A"), Node[Unit](jsonkey = "B")))
          )
        )
        val res =
          compareSingleGraphADTs[Unit, Unit, Unit](expectedADT, singleNodesOnly)
        assert(res.isRight)
      }
    }
    "parse a single graph with edges and nodes and the ADT" should {
      "contain a single graph with expected edges and nodes" in {
        val expectedADT = TopLevelSingleGraph(
          SimpleGraph[Unit, Unit, Unit](
            nodes =
              Nodes(List(Node[Unit](jsonkey = "A"), Node[Unit](jsonkey = "B"))),
            edges = List(SimpleEdge[Unit]("A", "B"))
          )
        )
        val res = compareSingleGraphADTs[Unit, Unit, Unit](
          expectedADT,
          singleGraphNodesEdges
        )
        assert(res.isRight)
      }
    }
    "parse a single graph with hyperedges and the ADT" should {
      "contain a HyperGraph instance with expected hyperedges and nodes" in {
        val expectedADT = TopLevelSingleGraph(
          UndirectedHyperGraph[Unit, Unit, Unit](
            nodes =
              Nodes(List(Node[Unit](jsonkey = "A"), Node[Unit](jsonkey = "B"))),
            edges = List(
              UndirectedHyperEdge(
                List("A", "B"),
                relation = Some("associated"),
                metadata = Some(())
              )
            )
          )
        )
        val res = compareSingleGraphADTs[Unit, Unit, Unit](
          expectedADT,
          singleGraphHyper
        )
        assert(res.isRight)
      }
    }
    "parse a complete example of a single graph and the ADT" should {
      "be a simple graph" in {
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

        val expectedADT = TopLevelSingleGraph(
          SimpleGraph[MetaData, MetaData, MetaData](
            directed = false,
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
                directed = false,
                Some("edge label"),
                Some(MetaData("values"))
              )
            )
          )
        )
        val res = compareSingleGraphADTs(expectedADT, completeSingle)
        assert(res.isRight)
      }
    }
  }

  private def compareSingleGraphADTs[M1, M2, M3](
      expectedADT: TopLevelSingleGraph[M1, M2, M3],
      parsePath: String
  )(implicit
      t1D: Decoder[M1],
      t2D: Decoder[M2],
      t3D: Decoder[M3]
  ): Either[ParsingFailure, Either[DecodingFailure, Assertion]] = {
    val source = scala.io.Source.fromFile(parsePath)
    val jsonContent =
      try {
        source.mkString
      } finally {
        source.close()
      }

    val parsedJSON = parse(jsonContent)

    for {
      p <- parsedJSON
    } yield {
      val adtRes = p.as[TopLevelSingleGraph[M1, M2, M3]]
      for {
        adt <- adtRes
      } yield {
        println(adt)
        assert(adt equals expectedADT)
      }

    }
  }
}
