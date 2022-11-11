package com.github.tizuck
package dot

import jsonGraphSchema.{DirectedHyperGraph, Node, SimpleGraph, TopLevelSingleGraph, UndirectedHyperGraph}

import scalax.collection.GraphEdge.{Bag, CollectionKind}
import scalax.collection.GraphPredef
import scalax.collection.edge.{LDiEdge, LDiHyperEdge, LHyperEdge}
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

  private def toScalaGraph[M1,M2,M3](graph:DirectedHyperGraph[M1,M2,M3]):
  scalax.collection.Graph[Node[M2],LDiHyperEdge] = {

    val nodes = graph.nodes.nodes

    val edges :List[LDiHyperEdge[Node[M2]]] = graph.edges.flatMap{e =>
      for{sourceNodeKey <- e.source} yield {
        val sourceNode = nodes
          .find(n => n.jsonkey.equals(sourceNodeKey))
          .getOrElse(Node[M2](jsonkey = sourceNodeKey))

         val targetNodes = e.target
           .map(targetKey => nodes
             .find(n => n.jsonkey.equals(targetKey))
             .getOrElse(Node[M2](jsonkey = targetKey)))

        val all : List[Node[M2]] = sourceNode :: targetNodes

        implicit val kind:CollectionKind = Bag
        LDiHyperEdge(all)(e)
      }
    }
    scalax.collection.Graph[Node[M2],LDiHyperEdge](edges : _*)
  }

  private def toScalaGraph[M1,M2,M3](graph:SimpleGraph[M1,M2,M3]):
  scalax.collection.Graph[Node[M2],LDiEdge] = {

    val nodes = graph.nodes.nodes

    import scalax.collection.edge.Implicits._

    val edges = graph.edges.map { e =>
      val node1: Node[M2] = nodes.find(p => p.jsonkey.equals(e.source)).get
      val node2: Node[M2] = nodes.find(p => p.jsonkey.equals(e.target)).get

      (node1 ~+> node2)(e)
    }
    scalax.collection.Graph[Node[M2],LDiEdge](edges : _*)
  }

  private def toScalaGraph[M1,M2,M3](graph:UndirectedHyperGraph[M1,M2,M3]):
  scalax.collection.Graph[Node[M2],LHyperEdge] = {

    val nodes = graph.nodes.nodes

    val edges:List[LHyperEdge[Node[M2]]] = graph.edges.map{ e =>
      val edgeNodes : List[Node[M2]] = e.nodes
        .map(nKey => nodes
          .find(n => n.jsonkey.equals(nKey))
          .getOrElse(Node[M2](jsonkey = nKey)))

      implicit val kind:CollectionKind = Bag
      LHyperEdge(edgeNodes)(e)
    }

    scalax.collection.Graph[Node[M2],LHyperEdge](edges : _*)
  }

  def apply[M1,M2,M3,E[+X] <: GraphPredef.EdgeLikeIn[X]](
                                                          topLevel: TopLevelSingleGraph[M1,M2,M3],
                                                          representationCtx: RepresentationCtx[Node[M2],E])
  :DotRepresentation = {

    import scalax.collection.io.dot._

    topLevel.graph match {
      case s:SimpleGraph[M1,M2,M3] =>
        val scalaGraph = toScalaGraph(s)
        val dotRep = scalaGraph.toDot(
          dotRoot = representationCtx.dotRoot,
          edgeTransformer = representationCtx.edgeTransformer.asInstanceOf[EdgeTransformer[Node[M2],LDiEdge]]
        )
        DotRepresentation(dotRep)

      case dhg:DirectedHyperGraph[M1,M2,M3] =>
        val scalaGraph = toScalaGraph(dhg)
        val dotRep = scalaGraph.toDot(
          dotRoot = representationCtx.dotRoot,
          edgeTransformer = representationCtx.edgeTransformer.asInstanceOf[EdgeTransformer[Node[M2], LDiHyperEdge]]
        )
        DotRepresentation(dotRep)

      case uhg:UndirectedHyperGraph[M1,M2,M3] =>
        val scalaGraph = toScalaGraph(uhg)
        val dotRep = scalaGraph.toDot(
          dotRoot = representationCtx.dotRoot,
          edgeTransformer = representationCtx.edgeTransformer.asInstanceOf[EdgeTransformer[Node[M2],LHyperEdge]]
        )
        DotRepresentation(dotRep)
    }
  }
}
