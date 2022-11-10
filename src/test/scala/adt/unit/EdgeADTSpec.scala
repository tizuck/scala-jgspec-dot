package com.github.tizuck
package adt.unit

import com.github.tizuck.jsonGraphSchema.{DirectedHyperEdge, Edge, SimpleEdge, UndirectedHyperEdge}
import org.scalatest.wordspec.AnyWordSpec
import io.circe.parser._

/**
 * Testing the ambiguous definitions of edges.
 * Using standard derived Decoders it is not possible
 * to distinguish between the different types of edges
 * since most fields are optional and therefore conform to
 * any edge instance.
 */
class EdgeADTSpec extends AnyWordSpec {
  val jsonSimpleEdge: String =
    """
      |{
      |  "source":"A",
      |  "target":"B"
      |}
      |""".stripMargin

  val jsonDirectedHyperEdge: String =
    """
      |{
      |  "source":["A","B"],
      |  "target":["A"]
      |}
      |""".stripMargin

  val jsonUndirectedHyperEdge : String =
    """
      |{
      |  "nodes":["A","B"]
      |}
      |""".stripMargin

  "An Edge parser" should {
    "parse a simple edge to the correct type" in {
      val jsonParse = parse(jsonSimpleEdge)

      for {p <- jsonParse} yield {
        val res = p.as[Edge[Unit]]
        for{r <- res} yield {
          println(r)
          assert(r.isInstanceOf[SimpleEdge[Unit]])
        }
      }
    }
    "parse a directedHyperEdge to the correct Type" in {
      val jsonParse = parse(jsonDirectedHyperEdge)

      for {p <- jsonParse} yield {
        val res = p.as[Edge[Unit]]
        for {r <- res} yield {
          println(r)
          assert(r.isInstanceOf[DirectedHyperEdge[Unit]])
        }
      }
    }
    "parse a undirectedHyperEdge to the correct Type" in {
      val jsonParse = parse(jsonUndirectedHyperEdge)

      for {p <- jsonParse} yield {
        val res = p.as[Edge[Unit]]
        for {r <- res} yield {
          println(r)
          assert(r.isInstanceOf[UndirectedHyperEdge[Unit]])
        }
      }
    }
  }
}
