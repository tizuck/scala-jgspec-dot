package com.github.tizuck

import io.circe.{Decoder, DecodingFailure, HCursor}

object jsonGraphSchema {

  sealed case class Nodes[T](nodes:List[Node[T]])
  sealed case class Node[T](label:Option[String] = None,metadata:Option[T] = None,jsonkey:String)

  sealed trait Edge[T] {
    val id:Option[String]

    val relation:Option[String]

    val directed:Boolean

    val label:Option[String]

    val metadata:Option[T]
  }
  sealed case class SimpleEdge[T](
                    source:String,
                    target:String,
                    override val id:Option[String] = None,
                    override val relation:Option[String] = None,
                    override val directed:Boolean = true,
                    override val label:Option[String] = None,
                    override val metadata:Option[T] = None
                    ) extends Edge[T]

  sealed case class DirectedHyperEdge[T](
                                        source:List[String],
                                        target:List[String],
                                        override val id: Option[String] = None,
                                        override val relation: Option[String] = None,
                                        override val directed: Boolean = true,
                                        override val label: Option[String] = None,
                                        override val metadata: Option[T] = None
                                        ) extends Edge[T]

  sealed case class UndirectedHyperEdge[T](
                                            nodes: List[String],
                                            override val id: Option[String] = None,
                                            override val relation: Option[String] = None,
                                            override val directed: Boolean = true,
                                            override val label: Option[String] = None,
                                            override val metadata: Option[T] = None
                                          ) extends Edge[T]

  sealed trait Graph[M1,M2,M3] {
    val id:Option[String]

    val label:Option[String]

    val directed:Boolean

    val tpe:Option[String]

    val metadata:Option[M1]

    val nodes:Nodes[M2]

    val edges:List[Edge[M3]]
  }
  sealed case class SimpleGraph[M1,M2,M3](
                    override val tpe:Option[String] = None,
                    override val metadata:Option[M1] = None,
                    override val nodes:Nodes[M2] = Nodes(List.empty[Node[M2]]),
                    override val id: Option[String] = None,
                    override val label: Option[String] = None,
                    override val directed: Boolean = true,
                    override val edges:List[SimpleEdge[M3]] = List.empty[SimpleEdge[M3]],
                  ) extends Graph[M1,M2,M3] {
  }

  sealed case class DirectedHyperGraph[M1,M2,M3](
                                                  override val tpe: Option[String] = None,
                                                  override val metadata: Option[M1] = None,
                                                  override val nodes: Nodes[M2] = Nodes(List.empty[Node[M2]]),
                                                  override val id: Option[String] = None,
                                                  override val label: Option[String] = None,
                                                  override val directed: Boolean = true,
                                                  override val edges: List[DirectedHyperEdge[M3]] = List.empty[DirectedHyperEdge[M3]]
                                                ) extends Graph[M1,M2,M3]

  sealed case class UndirectedHyperGraph[M1,M2,M3](
                                                    override val tpe: Option[String] = None,
                                                    override val metadata: Option[M1] = None,
                                                    override val nodes: Nodes[M2] = Nodes(List.empty[Node[M2]]),
                                                    override val id: Option[String] = None,
                                                    override val label: Option[String] = None,
                                                    override val directed: Boolean = false,
                                                    override val edges:List[UndirectedHyperEdge[M3]]
                                                  ) extends Graph[M1,M2,M3]

  sealed case class TopLevelSingleGraph[M1,M2,M3](graph: Graph[M1,M2,M3])
  sealed case class TopLevelMultipleGraphs[M1,M2,M3](graphs:List[Graph[M1,M2,M3]])




  implicit def toplevelSingleGraphDecoder[M1,M2,M3,E <: Edge[M3]](implicit t1decoder:Decoder[M1],
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

  implicit def topLevelMultipleGraphsDecoder[M1,M2,M3,E <: Edge[M3]](implicit t1decoder:Decoder[M1],
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

  import cats.syntax.functor._

  implicit def edgeDecoder[T](implicit tDecoder:Decoder[T]):Decoder[Edge[T]] = {
      val l:List[Decoder[Edge[T]]] = List[Decoder[Edge[T]]](
        Decoder[SimpleEdge[T]].widen,
        Decoder[DirectedHyperEdge[T]].widen,
        Decoder[UndirectedHyperEdge[T]].widen
      )
      l.reduceLeft(_ or _)
  }

  implicit def directedHyperEdge[T](implicit tDecoder:Decoder[T]):Decoder[DirectedHyperEdge[T]] = {
    (c:HCursor) => {
      for {
        id <- c.downField("id").as[Option[String]]
        source <- c.downField("source").as[List[String]]
        target <- c.downField("target").as[List[String]]
        relation <- c.downField("relation").as[Option[String]]
        directed <- c.downField("directed").as[Option[Boolean]]
        label <- c.downField("label").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T]]
      } yield {
        new DirectedHyperEdge[T](
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

  implicit def undirectedHyperEdge[T](implicit tDecoder:Decoder[T]):Decoder[UndirectedHyperEdge[T]] = {
    (c:HCursor) => {
      for {
        id <- c.downField("id").as[Option[String]]
        relation <- c.downField("relation").as[Option[String]]
        directed <- c.downField("directed").as[Option[Boolean]]
        label <- c.downField("label").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T]]
        nodes <- c.downField("nodes").as[List[String]]
      } yield {
        new UndirectedHyperEdge[T](
          id = id,
          relation = relation,
          directed = directed.getOrElse(true),
          label = label,
          metadata = metadata,
          nodes = nodes
        )
      }
    }
  }

  implicit def simpleEdgeDecoder[T1](implicit t1Decoder:Decoder[T1]):Decoder[SimpleEdge[T1]] = {
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

  implicit def graphDecoder[T1,T2,T3](implicit t1decoder: Decoder[T1],
                                                        t2decoder: Decoder[T2],
                                                        t3decoder: Decoder[T3]):Decoder[Graph[T1,T2,T3]] = {
    val l: List[Decoder[Graph[T1,T2,T3]]] = List[Decoder[Graph[T1,T2,T3]]](
      Decoder[SimpleGraph[T1,T2,T3]].widen,
      Decoder[DirectedHyperGraph[T1,T2,T3]].widen,
      Decoder[UndirectedHyperGraph[T1,T2,T3]].widen,
    )
    l.reduceLeft(_ or _)
  }

  implicit def undirectedHyperGraphDecoder[T1,T2,T3](implicit t1decoder: Decoder[T1],
                                                     t2decoder: Decoder[T2],
                                                     t3decoder: Decoder[T3]):
  Decoder[UndirectedHyperGraph[T1,T2,T3]] = {

    (c: HCursor) => {
      for {
        id <- c.downField("id").as[Option[String]]
        label <- c.downField("label").as[Option[String]]
        directed <- c.downField("directed").as[Option[Boolean]]
        tpe <- c.downField("type").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T1]]
        nodes <- c.downField("nodes").as[Option[Nodes[T2]]]
        edges <- c.downField("hyperedges").as[Option[List[UndirectedHyperEdge[T3]]]]
      } yield {
        new UndirectedHyperGraph[T1,T2,T3](
          id = id,
          label = label,
          directed = directed.getOrElse(false),
          tpe = tpe,
          metadata = metadata,
          nodes = nodes.getOrElse(Nodes(Nil)),
          edges = edges.getOrElse(Nil)
        )
      }
    }
  }
  implicit def directedHyperGraphEncoder[T1,T2,T3](implicit t1decoder: Decoder[T1],
                                                   t2decoder: Decoder[T2],
                                                   t3decoder: Decoder[T3]):Decoder[DirectedHyperGraph[T1,T2,T3]] = {
    (c: HCursor) => {
      for {
        id <- c.downField("id").as[Option[String]]
        label <- c.downField("label").as[Option[String]]
        directed <- c.downField("directed").as[Option[Boolean]]
        tpe <- c.downField("type").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T1]]
        nodes <- c.downField("nodes").as[Option[Nodes[T2]]]
        edges <- c.downField("hyperedges").as[Option[List[DirectedHyperEdge[T3]]]]
      } yield {
        new DirectedHyperGraph[T1,T2,T3](
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

  implicit def simpleGraphDecoder[T1,T2,T3](
                                       implicit t1decoder:Decoder[T1],
                                       t2decoder:Decoder[T2],
                                       t3decoder:Decoder[T3]) : Decoder[SimpleGraph[T1,T2,T3]] = {
    (c: HCursor) => {
      if(c.keys.exists(key => key.exists(s => s.equals("hyperedges")))) {
        Left(DecodingFailure("Hyperedges unaccepted member of simple graph",c.history))
      } else {
        for {
        id <- c.downField("id").as[Option[String]]
        label <- c.downField("label").as[Option[String]]
        directed <- c.downField("directed").as[Option[Boolean]]
        tpe <- c.downField("type").as[Option[String]]
        metadata <- c.downField("metadata").as[Option[T1]]
        nodes <- c.downField("nodes").as[Option[Nodes[T2]]]
        edges <- c.downField("edges").as[Option[List[SimpleEdge[T3]]]]
      } yield {
        new SimpleGraph[T1, T2, T3](
          id = id,
          label = label,
          directed = directed.getOrElse(true),
          tpe = tpe,
          metadata = metadata,
          nodes = nodes.getOrElse(Nodes(Nil)),
          edges = edges.getOrElse(Nil)
        )
      }}
    }
  }
}
