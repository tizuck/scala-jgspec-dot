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
package dot

import jsonGraphSchema.{
  DirectedHyperGraph,
  Node,
  SimpleGraph,
  TopLevelSingleGraph,
  UndirectedHyperGraph,
  TopLevelMultipleGraphs
}

import io.circe.parser

import scalax.collection.GraphEdge.{Bag, CollectionKind}
import scalax.collection.GraphPredef
import scalax.collection.edge.{LDiEdge, LDiHyperEdge, LHyperEdge}
import scalax.collection.io.dot.{
  DotRootGraph,
  EdgeTransformer,
  HyperEdgeTransformer,
  NodeTransformer
}

/** Container for the string representation of decoded JSON graph specification
  * files.
  *
  * Use the apply method to transform a
  * [[TopLevelSingleGraph TopLevelSingleGraph]] or
  * [[TopLevelMultipleGraphs TopLevelMultipleGraphs]] into a `String` of the Dot
  * file format.
  *
  * For a json file:
  * {{{
  *   {
  *     "graph":{
  *       "nodes":{
  *         "q1":{"label":"1"}
  *          "q2":{"label":"2"}
  *       },
  *       "edges":[
  *         {
  *           "source":"q1",
  *           "target":"q2",
  *           "label":"foo"
  *         }
  *      ]
  *     }
  *   }
  * }}}
  *
  * You can get the Dot representation by calling the [[parser.parse]] method
  * and process the parsed result. The Dot format is generating using the Dot
  * export of [[https://www.scala-graph.org/guides/dot.html Scala Graph]]. For
  * further information on how to create edge transformers and other details
  * about the Dot export look into their documentation. The following example
  * implements a simple edge transformer, that creates an edge by reading the
  * json keys and the label of a
  * [[com.github.tizuck.jsonGraphSchema.SimpleEdge SimpleEdge]].
  *
  * {{{
  * def edgeTransformer(innerEdge: scalax.collection.Graph[Node[Unit], LDiEdge]#EdgeT
  *  ): Option[(DotGraph, DotEdgeStmt)] = {
  *
  *   innerEdge.edge match {
  *    case LDiEdge(source, target, label) =>
  *      label match {
  *        case s: SimpleEdge[_] if s.label.nonEmpty =>
  *          Some((dotRoot,
  *              DotEdgeStmt(
  *                NodeId(source.toOuter.jsonkey),
  *                NodeId(target.toOuter.jsonkey),
  *                List(DotAttr(Id("label"), Id(s.label.get))))))
  *        case _ => None
  *      }
  *    case _ => None
  *  }}
  *
  * val rep = RepresentationCtx(
  *  dotRoot = DotRootGraph(directed = true, None),
  *  edgeTransformer)
  *
  * val parsed = parser.parse(json)
  * for { p <- parsed } yield {
  *   for { tpe <- p.as[TopLevelSingleGraph[Unit, Unit, Unit]] } yield {
  *     println(DotRepresentation(tpe, rep).dot)
  *   }
  * }
  * }}}
  *
  * This will produce the output:
  *
  * {{{
  * > digraph { q1 -> q2 [label = foo] }
  * }}}
  * @param dot
  *   Dot representation of a graph received in the form of a Json file.
  */
sealed case class DotRepresentation(dot: String)

object DotRepresentation {

  sealed case class RepresentationCtx[N, E[+X] <: GraphPredef.EdgeLikeIn[X]](
      dotRoot: DotRootGraph,
      edgeTransformer: EdgeTransformer[N, E],
      hyperEdgeTransformer: Option[HyperEdgeTransformer[N, E]] = None,
      cNodeTransformer: Option[NodeTransformer[N, E]] = None,
      iNodeTransformer: Option[NodeTransformer[N, E]] = None
  )

  private def toScalaGraph[M1, M2, M3](
      graph: DirectedHyperGraph[M1, M2, M3]
  ): scalax.collection.Graph[Node[M2], LDiHyperEdge] = {

    val nodes = graph.nodes.nodes

    val edges: List[LDiHyperEdge[Node[M2]]] = graph.edges.flatMap { e =>
      for { sourceNodeKey <- e.source } yield {
        val sourceNode = nodes
          .find(n => n.jsonkey.equals(sourceNodeKey))
          .getOrElse(Node[M2](jsonkey = sourceNodeKey))

        val targetNodes =
          try {
            e.target
              .map(targetKey =>
                nodes
                  .find(n => n.jsonkey.equals(targetKey))
                  .getOrElse(Node[M2](jsonkey = targetKey))
              )
          } catch {
            case n: NoSuchElementException =>
              throw new IllegalStateException(
                "Graph file not properly specified: There exists a node in an edge that" +
                  "does not exists in the nodes"
              )
          }

        val all: List[Node[M2]] = sourceNode :: targetNodes

        implicit val kind: CollectionKind = Bag
        LDiHyperEdge(all)(e)
      }
    }
    scalax.collection.Graph[Node[M2], LDiHyperEdge](edges: _*)
  }

  private def toScalaGraph[M1, M2, M3](
      graph: SimpleGraph[M1, M2, M3]
  ): scalax.collection.Graph[Node[M2], LDiEdge] = {

    val nodes = graph.nodes.nodes

    import scalax.collection.edge.Implicits._

    val edges =
      try {
        graph.edges.map { e =>
          val node1: Node[M2] = nodes.find(p => p.jsonkey.equals(e.source)).get
          val node2: Node[M2] = nodes.find(p => p.jsonkey.equals(e.target)).get

          (node1 ~+> node2)(e)
        }
      } catch {
        case n: NoSuchElementException =>
          throw new IllegalStateException(
            "Graph file not properly specified: There exists a node in an edge that" +
              "does not exists in the nodes"
          )
      }
    scalax.collection.Graph[Node[M2], LDiEdge](edges: _*)
  }

  private def toScalaGraph[M1, M2, M3](
      graph: UndirectedHyperGraph[M1, M2, M3]
  ): scalax.collection.Graph[Node[M2], LHyperEdge] = {

    val nodes = graph.nodes.nodes

    val edges: List[LHyperEdge[Node[M2]]] =
      try {
        graph.edges.map { e =>
          val edgeNodes: List[Node[M2]] = e.nodes
            .map(nKey =>
              nodes
                .find(n => n.jsonkey.equals(nKey))
                .getOrElse(Node[M2](jsonkey = nKey))
            )

          implicit val kind: CollectionKind = Bag
          LHyperEdge(edgeNodes)(e)
        }
      } catch {
        case n: NoSuchElementException =>
          throw new IllegalStateException(
            "Graph file not properly specified: There exists a node in an edge that" +
              "does not exists in the nodes"
          )
      }

    scalax.collection.Graph[Node[M2], LHyperEdge](edges: _*)
  }

  def apply[M1, M2, M3, E[+X] <: GraphPredef.EdgeLikeIn[X]](
      topLevel: TopLevelSingleGraph[M1, M2, M3],
      representationCtx: RepresentationCtx[Node[M2], E]
  ): DotRepresentation = {

    import scalax.collection.io.dot._

    topLevel.graph match {
      case s: SimpleGraph[M1, M2, M3] =>
        val scalaGraph = toScalaGraph(s)
        val dotRep = scalaGraph.toDot(
          dotRoot = representationCtx.dotRoot,
          edgeTransformer = representationCtx.edgeTransformer
            .asInstanceOf[EdgeTransformer[Node[M2], LDiEdge]],
          cNodeTransformer = representationCtx.cNodeTransformer
            .asInstanceOf[Option[NodeTransformer[Node[M2], LDiEdge]]],
          iNodeTransformer = representationCtx.iNodeTransformer
            .asInstanceOf[Option[NodeTransformer[Node[M2], LDiEdge]]],
          hEdgeTransformer = representationCtx.hyperEdgeTransformer
            .asInstanceOf[Option[HyperEdgeTransformer[Node[M2], LDiEdge]]]
        )
        DotRepresentation(dotRep)

      case dhg: DirectedHyperGraph[M1, M2, M3] =>
        val scalaGraph = toScalaGraph(dhg)
        val dotRep = scalaGraph.toDot(
          dotRoot = representationCtx.dotRoot,
          edgeTransformer = representationCtx.edgeTransformer
            .asInstanceOf[EdgeTransformer[Node[M2], LDiHyperEdge]],
          cNodeTransformer = representationCtx.cNodeTransformer
            .asInstanceOf[Option[NodeTransformer[Node[M2], LDiHyperEdge]]],
          iNodeTransformer = representationCtx.iNodeTransformer
            .asInstanceOf[Option[NodeTransformer[Node[M2], LDiHyperEdge]]],
          hEdgeTransformer = representationCtx.hyperEdgeTransformer
            .asInstanceOf[Option[HyperEdgeTransformer[Node[M2], LDiHyperEdge]]]
        )
        DotRepresentation(dotRep)

      case uhg: UndirectedHyperGraph[M1, M2, M3] =>
        val scalaGraph = toScalaGraph(uhg)
        val dotRep = scalaGraph.toDot(
          dotRoot = representationCtx.dotRoot,
          edgeTransformer = representationCtx.edgeTransformer
            .asInstanceOf[EdgeTransformer[Node[M2], LHyperEdge]],
          cNodeTransformer = representationCtx.cNodeTransformer
            .asInstanceOf[Option[NodeTransformer[Node[M2], LHyperEdge]]],
          iNodeTransformer = representationCtx.iNodeTransformer
            .asInstanceOf[Option[NodeTransformer[Node[M2], LHyperEdge]]],
          hEdgeTransformer = representationCtx.hyperEdgeTransformer
            .asInstanceOf[Option[HyperEdgeTransformer[Node[M2], LHyperEdge]]]
        )
        DotRepresentation(dotRep)
    }
  }
}
