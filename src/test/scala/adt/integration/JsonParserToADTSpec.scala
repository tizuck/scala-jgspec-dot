package com.github.tizuck
package adt.integration

import jsonGraphSchema.{Node, Nodes, SimpleEdge, SimpleGraph, TopLevelSingleGraph, UndirectedHyperEdge, UndirectedHyperGraph}

import io.circe.Decoder
import io.circe.parser._
import org.scalatest.wordspec.AnyWordSpec

class JsonParserToADTSpec extends AnyWordSpec {

  val empty = "src/test/resources/empty.json"
  val singleNodesOnly = "src/test/resources/nodesonlysingle.json"
  val singleGraphNodesEdges = "src/test/resources/nodesedgessingle.json"
  val singleGraphHyper = "src/test/resources/hyperedgessingle.json"

  "A Json Parser" should {
    "parse an empty single graph and the ADT" should {
      "contain an empty graph" in {
        val expectedADT = TopLevelSingleGraph(SimpleGraph[Unit,Unit,Unit]())
        compareSingleGraphADTs[Unit,Unit,Unit](expectedADT,empty)
      }
    }
    "parse a single graph with nodes only and the ADT" should {
      "contain a single graph with the two expected nodes" in {
        val expectedADT = TopLevelSingleGraph(
          SimpleGraph[Unit,Unit,Unit](
            nodes = Nodes(List(
              Node[Unit](jsonkey = "A"),
              Node[Unit](jsonkey = "B")
            ))
          )
        )
        compareSingleGraphADTs[Unit,Unit,Unit](expectedADT,singleNodesOnly)
      }
    }
    "parse a single graph with edges and nodes and the ADT" should {
      "contain a single graph with expected edges and nodes" in {
        val expectedADT = TopLevelSingleGraph(
          SimpleGraph[Unit,Unit,Unit](
            nodes = Nodes(List(
                Node[Unit](jsonkey = "A"),
                Node[Unit](jsonkey = "B")
              )),
            edges = List(
              SimpleEdge[Unit]("A","B")
            )
          )
        )
        compareSingleGraphADTs[Unit,Unit,Unit](expectedADT,singleGraphNodesEdges)
      }
    }
    "parse a single graph with hyperedges and the ADT" should {
      "contain a HyperGraph instance with expected hyperedges and nodes" in {
        val expectedADT = TopLevelSingleGraph(
          UndirectedHyperGraph[Unit,Unit,Unit](
            nodes = Nodes(List(
              Node[Unit](jsonkey = "A"),
              Node[Unit](jsonkey = "B")
            )),
            edges = List(
              UndirectedHyperEdge(
                List("A","B"),
                relation = Some("associated")
              )
            )
          )
        )
        compareSingleGraphADTs[Unit,Unit,Unit](expectedADT,singleGraphHyper)
      }
    }
  }

  private def compareSingleGraphADTs[M1, M2, M3](expectedADT: TopLevelSingleGraph[M1, M2, M3],parsePath:String)(
    implicit t1D: Decoder[M1],
    t2D: Decoder[M2],
    t3D: Decoder[M3]): Unit = {
    val source = scala.io.Source.fromFile(parsePath)
    val jsonContent = try {
      source.mkString
    } finally {
      source.close()
    }

    val parsedJSON = parse(jsonContent)

    val _ = for {
      p <- parsedJSON
    } yield {
      val adtRes = p.as[TopLevelSingleGraph[M1, M2, M3]]
      for {
        adt <- adtRes
      } yield {
        println(adt)
        println(expectedADT)
        assert(adt equals expectedADT)
      }

    }
  }
}
