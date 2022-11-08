package com.github.tizuck

import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor}

object jsonGraphSchema {

  sealed case class Nodes[T](nodes:List[Node[T]])
  sealed case class Node[T](label:Option[String] = None,metadata:Option[T] = None,jsonkey:String)

  sealed case class SimpleEdge[T](
                    id:Option[String] = None,
                    source:String,
                    target:String,
                    relation:Option[String] = None,
                    directed:Boolean = true,
                    label:Option[String] = None,
                    metadata:Option[T] = None
                    )

  sealed case class Graph[M1,M2,M3](
                    id:Option[String] = None,
                    label:Option[String] = None,
                    directed:Boolean = true,
                    tpe:Option[String] = None,
                    metadata:Option[M1],
                    nodes:Nodes[M2],
                    edges:List[SimpleEdge[M3]]
                  )

  sealed case class TopLevelSingleGraph[M1,M2,M3](graph: Graph[M1,M2,M3])
  sealed case class TopLevelMultipleGraphs[M1,M2,M3](graphs:List[Graph[M1,M2,M3]])

  implicit def toplevelSingleGraphDecoder[M1,M2,M3](implicit t1decoder:Decoder[M1],
                                                    t2decoder:Decoder[M2],
                                                    t3decoder:Decoder[M3]):Decoder[TopLevelSingleGraph[M1,M2,M3]] = {
    (c:HCursor) => {
      for {
        graph <- c.downField("graph").as[Graph[M1,M2,M3]]
      } yield {
        TopLevelSingleGraph(graph)
      }
    }
  }

  implicit def topLevelMultipleGraphsDecoder[M1,M2,M3](implicit t1decoder:Decoder[M1],
                                                         t2decoder:Decoder[M2],
                                                         t3decoder:Decoder[M3]):
  Decoder[TopLevelMultipleGraphs[M1,M2,M3]] = {
    (c:HCursor) => {
      for {
        graphs <- c.downField("graphs").as[List[Graph[M1,M2,M3]]]
      } yield {
        TopLevelMultipleGraphs(graphs)
      }
    }
  }

  implicit def edgeDecoder[T1](implicit t1Decoder:Decoder[T1]):Decoder[SimpleEdge[T1]] = {
    (c: HCursor) => {
      for {
        id <- c.downField("id").as[Option[String]]
        source <- c.downField("source").as[String]
        target <- c.downField("target").as[String]
        relation <- c.downField("relation").as[Option[String]]
        directed <- c.downField("directed").as[Option[Boolean]]
        label <- c.downField("label").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T1]]
      } yield {
        new SimpleEdge[T1](
          id = id,
          source = source,
          target = target,
          relation = relation,
          directed = directed.getOrElse(true),
          label = label,
          metadata = metadata
        )
      }
    }
  }

  implicit def nodeDecoder[T1](implicit t1Decoder:Decoder[T1]):Decoder[Node[T1]] = {
    (c:HCursor) => {
      for {
        label <- c.downField("label").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T1]]
      } yield {
        new Node[T1](
          label = label,
          metadata = metadata,
          //Value is initialized empty and set in a higher level decoder who knows about the
          //value of the key
          jsonkey = ""
        )
      }
    }
  }

  implicit def nodesDecoder[T1](implicit t1Decoder:Decoder[T1]):Decoder[Nodes[T1]] = {
    (c: HCursor) => {
      val keys = c.keys.getOrElse(Nil)

      val nodes = for {key <- keys} yield {
        for {
          node <- c.downField(key).as[Node[T1]]
        } yield {
          node.copy(jsonkey = key)
        }
      }

      nodes.foldLeft(Right[DecodingFailure, Nodes[T1]](Nodes(List.empty[Node[T1]])).withLeft) {
        case (acc, value) =>
          acc match {
            case l@Left(_) => l
            case Right(v) =>
              value match {
                case l@Left(_) => Left(DecodingFailure(l.value.reason, c.history))
                case Right(value) => Right(v.copy(nodes = v.nodes ++ List(value)))
              }
          }
      }
    }
  }

  implicit def graphDecoder[T1,T2,T3](
                                       implicit t1decoder:Decoder[T1],
                                       t2decoder:Decoder[T2],
                                       t3decoder:Decoder[T3]) : Decoder[Graph[T1,T2,T3]] = {
    (c: HCursor) => {
       for {
        id <- c.downField("id").as[Option[String]]
        label <- c.downField("label").as[Option[String]]
        directed <- c.downField("directed").as[Option[Boolean]]
        tpe <- c.downField("type").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T1]]
        nodes <- c.downField("nodes").as[Option[Nodes[T2]]]
        edges <- c.downField("edges").as[Option[List[SimpleEdge[T3]]]]
      } yield {
        new Graph[T1, T2, T3](
          id = id,
          label = label,
          directed = directed.getOrElse(true),
          tpe = tpe,
          metadata = metadata,
          nodes = nodes.getOrElse(Nodes(Nil)),
          edges = edges.getOrElse(Nil)
        )
      }
    }
  }
}
