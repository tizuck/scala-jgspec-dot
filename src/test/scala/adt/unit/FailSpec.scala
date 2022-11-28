/*
 * Copyright 2022 Tilman Zuckmantel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.tizuck
package adt.unit

import dot.DotRepresentation
import dot.DotRepresentation.RepresentationCtx
import jsonGraphSchema.{Node, Nodes, SimpleEdge, TopLevelSingleGraph}

import io.circe.parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot.{DotAttr, DotEdgeStmt, DotGraph, DotRootGraph, Id, NodeId}

class FailSpec extends AnyWordSpec with Matchers {
  "A JSON file that is not specified according to the JSON graph specification v2" when {
    "a specification deviation in a node field with a wrong type for a label" should {
      "calculate a DecodingFailure" in {
        val jsonErr =
          """
            |{
            |  "q1":{"label":1},
            |  "q2":{}
            |}
            |""".stripMargin

        val parseResCon = parser.parse(jsonErr)
        for { parseRes <- parseResCon } yield {
          val decodedCon = parseRes.as[Nodes[Unit]]
          decodedCon should be(Symbol("left"))
        }
      }
    }
    "a node is mentioned in the edges that is not defined in the nodes section" should {
      "throw an exception about malformed node section when trying to transform into DotRepresentation" in {
        val jsonErr =
          """
            |{
            |   "graph":{
            |      "nodes":{
            |        "q1":{"label":"1"}
            |      },
            |      "edges":[
            |        {
            |          "source":"q1",
            |           "target":"q2"
            |        }
            |      ]
            |    }
            | }
            |""".stripMargin

        val dotRoot = DotRootGraph(directed = true, None)

        def edgeTransformer(
            innerEdge: scalax.collection.Graph[Node[Unit], LDiEdge]#EdgeT
        ): Option[(DotGraph, DotEdgeStmt)] = {
          innerEdge.edge match {
            case LDiEdge(source, target, label) =>
              label match {
                case s: SimpleEdge[_] if s.label.nonEmpty =>
                  Some(
                    (
                      dotRoot,
                      DotEdgeStmt(
                        NodeId(source.toOuter.jsonkey),
                        NodeId(target.toOuter.jsonkey),
                        List(DotAttr(Id("label"), Id(s.label.get)))
                      )
                    )
                  )
                case _ => None
              }
            case _ => None
          }
        }

        val rep = RepresentationCtx(
          dotRoot = DotRootGraph(directed = true, None),
          edgeTransformer
        )

        val parseResCon = parser.parse(jsonErr)
        for { parseRes <- parseResCon } yield {
          for {
            tpe <- parseRes.as[TopLevelSingleGraph[Unit, Unit, Unit]]
          } yield {
            an[IllegalStateException] should be thrownBy DotRepresentation(
              tpe,
              rep
            ).dot
          }
        }
      }
    }
  }
}
