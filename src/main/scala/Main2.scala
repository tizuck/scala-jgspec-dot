package com.github.tizuck

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._

object Main2 extends App {
  val graph = Graph[String, DiEdge]("A" ~> "B")

  val dotRoot =
    DotRootGraph(directed = true, id = Some(Id("HTML-like labels example")))

  val edgeTransformer
      : Graph[String, DiEdge]#EdgeT => Option[(DotGraph, DotEdgeStmt)] = e => {
    e.toOuter match {
      case DiEdge("A", target) =>
        Some(
          (
            dotRoot,
            DotEdgeStmt(
              NodeId("A"),
              NodeId(target),
              List(DotAttr(Id("label"), Id("<<b>A</b>>")))
            )
          )
        )
    }
  }

  println(graph.toDot(dotRoot, edgeTransformer))
}
