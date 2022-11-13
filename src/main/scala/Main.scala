package com.github.tizuck

import dot.DotRepresentation
import dot.DotRepresentation.RepresentationCtx

import io.circe.parser._
import scalax.collection.edge.{LDiEdge, LDiHyperEdge}
import scalax.collection.io.dot.{DotAttr, DotAttrStmt, DotEdgeStmt, DotGraph, DotNodeStmt, DotRootGraph, DotSubGraph, Elem, Id, NodeId}


object Main extends App {
  val json =
    """
      |{
      |  "graph": {
      |    "directed": true,
      |    "type": "interface automaton",
      |    "metadata": {
      |      "ctx":{
      |        "transactionId":0,
      |        "spawnId":0
      |      }
      |    },
      |    "nodes": {
      |      "q0":{
      |        "label":"q0",
      |        "metadata":{
      |          "subgraph":true
      |        }
      |      },
      |      "q1":{
      |        "label":"q1",
      |        "metadata":{
      |          "subgraph":true
      |        }
      |      },
      |      "q2":{
      |        "label":"q2",
      |        "metadata":{
      |          "subgraph":true
      |        }
      |      },
      |      "q3":{
      |        "label":"q3",
      |        "metadata":{
      |          "subgraph":false
      |        }
      |      }
      |    },
      |    "edges": [
      |      {
      |        "source": "q0",
      |        "target": "q1",
      |        "directed": true,
      |        "label": "req"
      |	    },
      |	    {
      |	    "source": "q1",
      |	    "target": "q2",
      |	    "directed": true,
      |	    "label":"read"
      |	    },
      |	    {
      |	    "source": "q2",
      |	    "target": "q0",
      |	    "directed": true,
      |	    "label":"snd"
      |	    },
      |     {
      |	    "source": "q2",
      |	    "target": "q3",
      |	    "directed": true,
      |	    "label":"snd2"
      |	    }
      |    ]
      |  }
      |}
      |""".stripMargin

  import jsonGraphSchema._

  sealed case class GraphContext(transactionId:Int,spawnId:Int)
  sealed case class GraphMetadata(ctx:GraphContext)

  sealed case class NodeMetaData(subgraph:Boolean)

  val parseResult = parse(json)

  import io.circe.generic.auto._

  for {
    p <- parseResult
  } yield {
    val form = p.as[TopLevelSingleGraph[GraphMetadata,NodeMetaData,Unit]]
    for {
      f <- form
    } yield {
      println(f)

      val dotRoot = DotRootGraph(
        id = f.graph.id.map(i => Id(i)),
        directed = f.graph.directed,
      )

      println(dotRoot.id)
      println(dotRoot.directed)

      def edgeTransformer(innerEdge:scalax.collection.Graph[Node[NodeMetaData],LDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
        innerEdge.edge match {
          case LDiEdge(source,target,label) => label match {
            case e:Edge[_] =>
              Some(
                (dotRoot,
                  DotEdgeStmt(
                    NodeId(source.toOuter.jsonkey),
                    NodeId(target.toOuter.jsonkey),
                    (if(e.label.nonEmpty) List(DotAttr(Id("label"),Id(e.label.get.toString))) else Nil)
                  )))
          }
        }
      }

      def nodeTransformer(innerNode:scalax.collection.Graph[Node[NodeMetaData],LDiEdge]#NodeT)
      :Option[(DotGraph,DotNodeStmt)] = {

        if(innerNode.toOuter.metadata.exists(m => m.subgraph)){
          Some(
            dotRoot,DotNodeStmt(NodeId(innerNode.toOuter.jsonkey),Seq(DotAttr(Id("color"),Id("blue"))))
          )
        } else {
          None
        }
      }

      val ctx = RepresentationCtx[Node[NodeMetaData],LDiEdge](
        dotRoot = dotRoot,
        edgeTransformer= edgeTransformer,
        cNodeTransformer = Some(nodeTransformer)
      )

      println(DotRepresentation(f,ctx))
    }
  }
}
