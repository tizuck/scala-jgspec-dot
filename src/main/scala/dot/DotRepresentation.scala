package com.github.tizuck
package dot

import jsonGraphSchema.{SimpleGraph, Node, TopLevelSingleGraph}

import scalax.collection.GraphPredef
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot.{DotRootGraph, EdgeTransformer, HyperEdgeTransformer, NodeTransformer}

sealed case class DotRepresentation(dot:String)

object DotRepresentation {

   sealed case class RepresentationCtx[N,E[+X] <: GraphPredef.EdgeLikeIn[X]]
   (
     dotRoot:DotRootGraph,
     edgeTransformer:EdgeTransformer[N,E],
     hyperEdgeTransformer:Option[HyperEdgeTransformer[N,E]] = None,
     cNodeTransformer:Option[NodeTransformer[N,E]] = None,
     iNodeTransformer:Option[NodeTransformer[N,E]] = None)

  def apply[M1,M2,M3](topLevel: TopLevelSingleGraph[M1,M2,M3], representationCtx: RepresentationCtx[Node[M2],LDiEdge]):DotRepresentation = {
    def toScalaGraph(graph:SimpleGraph[M1,M2,M3]):scalax.collection.Graph[Node[M2],LDiEdge] = {
      val nodes = graph.nodes.nodes

      import scalax.collection.edge.Implicits._

      val edges = graph.edges.map { e =>
        val node1: Node[M2] = nodes.find(p => p.jsonkey.equals(e.source)).get
        val node2: Node[M2] = nodes.find(p => p.jsonkey.equals(e.target)).get

        (node1 ~+> node2) (e.label.getOrElse(""))
      }

      scalax.collection.Graph[Node[M2],LDiEdge](edges : _*)
    }
    import scalax.collection.io.dot._

    DotRepresentation(
      toScalaGraph(topLevel.graph).toDot(representationCtx.dotRoot,representationCtx.edgeTransformer)
    )
  }
}
