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

import dot.DotRepresentation
import dot.DotRepresentation.RepresentationCtx
import jsonGraphSchema.{Node, SimpleEdge, TopLevelSingleGraph}

import io.circe.parser
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot.{
  DotAttr,
  DotEdgeStmt,
  DotGraph,
  DotRootGraph,
  Id,
  NodeId
}

object Main extends App {
  val json =
    """
      |{
      |       "graph":{
      |         "nodes":{
      |           "q1":{"label":"1"},
      |           "q2":{"label":"2"}
      |         },
      |         "edges":[
      |           {
      |            "source":"q1",
      |             "target":"q2",
      |             "label":"foo"
      |           }
      |        ]
      |       }
      |    }
      |""".stripMargin

  sealed case class MetaData(meta: Int)

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

  val parsed = parser.parse(json)
  for { p <- parsed } yield {
    for { tpe <- p.as[TopLevelSingleGraph[Unit, Unit, Unit]] } yield {
      println(DotRepresentation(tpe, rep).dot)
    }
  }

}
