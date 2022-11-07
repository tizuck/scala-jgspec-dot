package com.github.tizuck

import com.github.tizuck.dot.DotRepresentation
import com.github.tizuck.dot.DotRepresentation.RepresentationCtx
import io.circe._
import io.circe.parser._
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor}
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot.{DotAttr, DotEdgeStmt, DotGraph, DotRootGraph, EdgeTransformer, Id, NodeId}


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
      |        "label":"q0"
      |      },
      |      "q1":{
      |        "label":"q1"
      |      },
      |      "q2":{
      |        "label":"q2"
      |      }
      |    },
      |    "edges": [
      |      {
      |        "source": "q0",
      |        "target": "q1",
      |        "directed": true,
      |        "label": "req"
      |	  },
      |	  {
      |	    "source": "q1",
      |	    "target": "q2",
      |	    "directed": true,
      |	    "label":"read"
      |	  },
      |	  {
      |	    "source": "q2",
      |	    "target": "q0",
      |	    "directed": true,
      |	    "label":"snd"
      |	  }
      |    ]
      |  }
      |}
      |""".stripMargin

  import jsonGraphSchema._

  sealed case class GraphContext(transactionId:Int,spawnId:Int)
  sealed case class GraphMetadata(ctx:GraphContext)

  val parseResult = parse(json)

  import io.circe.generic.auto._, io.circe.syntax._

  for {
    p <- parseResult
  } yield {
    val form = p.as[TopLevelSingleGraph[GraphMetadata,Unit,Unit]]
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

      def edgeTransformer(innerEdge:scalax.collection.Graph[Node[Unit],LDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
        innerEdge.edge match {
          case LDiEdge(source,target,label) => label match {
            case l:String =>
              Some(
                (dotRoot,
                  DotEdgeStmt(
                    NodeId(source.toOuter.jsonkey),
                    NodeId(target.toOuter.jsonkey),
                    (if(l.nonEmpty) List(DotAttr(Id("label"),Id(label.toString))) else Nil)
                  )))
          }
        }
      }

      val ctx = RepresentationCtx[Node[Unit],LDiEdge](dotRoot = dotRoot, edgeTransformer= edgeTransformer)

      println(DotRepresentation(f,ctx))
    }
  }
}
