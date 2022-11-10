package com.github.tizuck
package adt.unit

import jsonGraphSchema._

import io.circe.parser._
import io.circe.{Decoder, DecodingFailure, Json, ParsingFailure}
import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec

/**
 * Test cases for [[com.github.tizuck.jsonGraphSchema.graphDecoder]].
 * This Decoder is responsible for decoding the three edge types
 * [[com.github.tizuck.jsonGraphSchema.SimpleGraph]],[[com.github.tizuck.jsonGraphSchema.DirectedHyperGraph]]
 * and [[com.github.tizuck.jsonGraphSchema.UndirectedHyperGraph]].
 *
 * The issue with the three graphs is that they have many common values and
 * even more optional fields. Since the standard Decoder for [[Option]] values
 * accepts an instance of [[io.circe.DecodingFailure]] and produces [[None]]
 * as a result we need to implement our own decoders that strictly fail when
 * a value can not be parsed. To be completely clear, the following examples
 * illustrates the issue:
 *
 * {{{
 *   "graph":{
 *     "hyperedges":[{nodes:["A"]}]
 *   }
 * }}}
 *
 * If we use a Decoder for [[com.github.tizuck.jsonGraphSchema.DirectedHyperGraph]]
 * that uses standard Optional Decoders then the field of hyperedges will fail to parse, but because
 * the Decoder will return [[None]], the graph will still be interpreted as an instance of
 * [[com.github.tizuck.jsonGraphSchema.DirectedHyperGraph]] with an empty edge field. Instead we want
 * the decoder to fail and receive an instance of [[com.github.tizuck.jsonGraphSchema.UndirectedHyperGraph]].
 *
 * Further if no edges or nodes are specified then the type of the graph is ambiguous.
 * We expect it here to be parsed as an instance of [[com.github.tizuck.jsonGraphSchema.SimpleGraph]].
 */
class GraphADTSpec extends AnyWordSpec {
  val jsonEmpty: String =
    """
      |{}
      |""".stripMargin

  val jsonId:String =
  """
      |{"id":"1"}
      |""".stripMargin

  val jsonSimpleGraphImplicitMax:String =
    """
      |{"id":"1","label":"foo","directed":true,"type":"ia","metadata":{}}
      |""".stripMargin

  val jsonMinimalDirectedHyperGraph:String =
    """
      |{
      |    "id":"1",
      |    "hyperedges":[
      |      {
      |        "source":["A"],
      |        "target":["A"]
      |      }
      |    ]
      |  }
      |""".stripMargin

  val jsonMinimalUndirectedHyperGraph:String =
    """
      |{
      |   "hyperedges": [
      |    {
      |      "nodes": [ "A" ]
      |    }]
      | }
      |""".stripMargin

  "A graph parser" should {
    "parse an empty graph and SimpleGraph as a result is expected" in {
      val jsonParse = parse(jsonEmpty)
      val res = assertSimpleGraph[Unit,Unit,Unit](jsonParse,identity)
      assert(res.isRight)
    }

    "parse a graph with just an id field as a SimpleGraph instance" in {
      val jsonParse = parse(jsonId)
      val res = assertSimpleGraph[Unit,Unit,Unit](jsonParse,identity)
      assert(res.isRight)
    }

    "parse a graph with all fields that are in every graph as a simple graph" in {
      val jsonParse = parse(jsonSimpleGraphImplicitMax)
      val res = assertSimpleGraph[Unit,Unit,Unit](jsonParse,identity)
      assert(res.isRight)
    }

    "parse a graph with minimal fields to be identified as a directed hyper graph accordingly" in {
      val jsonParse = parse(jsonMinimalDirectedHyperGraph)
      val res = assertHyperGraph[Unit,Unit,Unit](jsonParse,identity)
      //Result is not a parsing mistake
      assert(res.isRight)
    }

    "parse a graph with minimal fields to be identified as a undirected hyper graph accordingly" in {
      val jsonParse = parse(jsonMinimalUndirectedHyperGraph)
      val res = assertUndirectedHyperGraph[Unit,Unit,Unit](jsonParse,identity)
      //Result is not a parsing mistake
      assert(res.isRight)

    }
  }











  private def assertSimpleGraph[M1,M2,M3](jsonParse: Either[ParsingFailure, Json],bf:Boolean => Boolean)(
    implicit m1Dec:Decoder[M1],
    m2Dec:Decoder[M2],
    m3Dec:Decoder[M3]):
  Either[ParsingFailure, Either[DecodingFailure, Assertion]] = {

    for{p <- jsonParse} yield {
      val res = p.as[Graph[M1,M2,M3]]
      for{res <- res}yield{
        assert(bf(res.isInstanceOf[SimpleGraph[M1,M2,M3]]),s"Instead type is: ${res.getClass}")
      }
    }
  }

  private def assertHyperGraph[M1,M2,M3](jsonParse: Either[ParsingFailure, Json],bf:Boolean => Boolean)(
    implicit m1Dec:Decoder[M1],
    m2Dec:Decoder[M2],
    m3Dec:Decoder[M3]):
  Either[ParsingFailure, Either[DecodingFailure, Assertion]] = {

    for{p <- jsonParse} yield {
      val res = p.as[Graph[M1,M2,M3]]
      for{res <- res}yield{
        println(res)
        assert(bf(res.isInstanceOf[DirectedHyperGraph[M1,M2,M3]]),s"Instead type is: ${res.getClass}")
      }
    }
  }

  private def assertUndirectedHyperGraph[M1,M2,M3](jsonParse: Either[ParsingFailure, Json],bf:Boolean => Boolean)(
    implicit m1Dec:Decoder[M1],
    m2Dec:Decoder[M2],
    m3Dec:Decoder[M3]):
  Either[ParsingFailure, Either[DecodingFailure, Assertion]] = {
    for{p <- jsonParse} yield {
      val res = p.as[Graph[M1,M2,M3]]
      for{res <- res}yield{
        println(res)
        assert(bf(res.isInstanceOf[UndirectedHyperGraph[M1,M2,M3]]),s"Instead type is: ${res.getClass}")
      }
    }
  }


}
