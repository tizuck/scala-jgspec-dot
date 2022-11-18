package com.github.tizuck
package adt.unit

import jsonGraphSchema.{
  DirectedHyperEdge,
  Edge,
  SimpleEdge,
  UndirectedHyperEdge
}

import io.circe.parser._
import io.circe.{Decoder, Json, ParsingFailure}
import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec

/** Test cases for [[com.github.tizuck.jsonGraphSchema.edgeDecoder]]. This
  * Decoder is responsible for decoding the three edge types
  * [[com.github.tizuck.jsonGraphSchema.SimpleEdge]],[[com.github.tizuck.jsonGraphSchema.DirectedHyperEdge]]
  * and [[com.github.tizuck.jsonGraphSchema.UndirectedHyperEdge]].
  *
  * Test cases cover one test for each edge type and checking if the parsed type
  * is as expected. Further tests ensure that it is not possible to interpret a
  * parsed result as a wrong type.
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

  val jsonUndirectedHyperEdge: String =
    """
      |{
      |  "nodes":["A","B"]
      |}
      |""".stripMargin

  "An Edge parser" should {
    "parse a simple edge to the correct type" in {
      val jsonParse = parse(jsonSimpleEdge)

      for { p <- jsonParse } yield {
        val res = p.as[Edge[Unit]]
        for { r <- res } yield {
          assert(r.isInstanceOf[SimpleEdge[Unit]])
        }
      }
    }
    "parse a directedHyperEdge to the correct Type" in {
      val jsonParse = parse(jsonDirectedHyperEdge)

      for { p <- jsonParse } yield {
        val res = p.as[Edge[Unit]]
        for { r <- res } yield {
          assert(r.isInstanceOf[DirectedHyperEdge[Unit]])
        }
      }
    }
    "parse a undirectedHyperEdge to the correct Type" in {
      val jsonParse = parse(jsonUndirectedHyperEdge)

      for { p <- jsonParse } yield {
        val res = p.as[Edge[Unit]]
        for { r <- res } yield {
          assert(r.isInstanceOf[UndirectedHyperEdge[Unit]])
        }
      }
    }
    "not accept parsing a simple edge as a directedHyperEdge" in {
      val jsonParse = parse(jsonSimpleEdge)

      val _ = assertWrongType[DirectedHyperEdge[Unit]](jsonParse)
    }
    "not accept parsing a simple edge as a undirectedHyperEdge" in {
      val jsonParse = parse(jsonSimpleEdge)

      val _ = assertWrongType[UndirectedHyperEdge[Unit]](jsonParse)
    }
    "not accept parsing a directedHyperEdge as a simple edge" in {
      val jsonParse = parse(jsonDirectedHyperEdge)

      val _ = assertWrongType[SimpleEdge[Unit]](jsonParse)
    }
    "not accept parsing a directedHyperEdge as an undirectedHyperEdge" in {
      val jsonParse = parse(jsonDirectedHyperEdge)

      val _ = assertWrongType[UndirectedHyperEdge[Unit]](jsonParse)
    }
    "not accept parsing an undirectedHyperEdge as a Simple Edge" in {
      val jsonParse = parse(jsonUndirectedHyperEdge)

      val _ = assertWrongType[SimpleEdge[Unit]](jsonParse)
    }
    "not accept parsing an undirectedHyperEdge as a DirectedHyperEdge" in {
      val jsonParse = parse(jsonUndirectedHyperEdge)

      val _ = assertWrongType[DirectedHyperEdge[Unit]](jsonParse)
    }
  }

  private def assertWrongType[T](
      jsonParse: Either[ParsingFailure, Json]
  )(implicit tDec: Decoder[T]): Either[ParsingFailure, Assertion] = {
    for { p <- jsonParse } yield {
      val res = p.as[T]
      println(res)
      assert(res.isLeft)
    }
  }
}
