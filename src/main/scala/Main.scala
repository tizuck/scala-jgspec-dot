package com.github.tizuck

import dot.DotRepresentation
import dot.DotRepresentation.RepresentationCtx

import io.circe.{Decoder, DecodingFailure, HCursor}
import io.circe.parser._
import scalax.collection.edge.{LDiEdge, LDiHyperEdge}
import scalax.collection.io.dot.{DotAttr, DotAttrStmt, DotEdgeStmt, DotGraph, DotNodeStmt, DotRootGraph, DotSubGraph, Elem, Id, NodeId}


object Main extends App {

  import jsonGraphSchema._

  case class GraphContext(spawnId:Int,transactionId:Int,keyValRef:List[Int])
  case class GraphMetaData(ctx:GraphContext)
  trait Event {
    val operationCtx:Context
  }
  case class Context(spawnId:Int,transactionId:Int)
  case class Variable(name:String,ctx:Context)
  case class ReceiveEventId(variable:Variable, override val operationCtx: Context) extends Event
  case class ReadVar(variable: Variable, keyValRef:Int, resultVariable:Variable, override val operationCtx: Context) extends Event
  case class EventSendVarId(override val operationCtx: Context,variableLookUp:Variable) extends Event

  import io.circe.generic.auto._

  case class Row(cells:List[String]){
    def toHTML:String = {
      s"<tr>${cells.map(c => s"<td>$c</td>").mkString}</tr>"
    }

    def appendCell(cell:String):Row = this.copy(cells = cells.appended(cell))
  }

  object Row {
    def empty:Row = Row(Nil)
  }
  case class Table(rows:List[Row]){
    def toHTML:String = {
      s"<<table>${rows.map(_.toHTML).mkString}</table>>"
    }
    def appendRow(row:Row):Table = {
      this.copy(rows = rows.appended(row))
    }
  }

  object Table {
    def empty:Table = Table(Nil)
  }



  implicit val metaEventDecoder:Decoder[Event] = {
    (c:HCursor) => {
      val res = for {
        tpe <- c.downField("tpe").as[String]
      } yield {
        val event = tpe match {
          case "ReceiveEventID" =>
            for {
              operationContext <- c.downField("operationCtx").as[Context]
              variable <- c.downField("variable").as[Variable]
            } yield {
              ReceiveEventId(variable,operationContext).asInstanceOf[Event]
            }
          case "ReadWithVar" =>
            for {
              operationContext <- c.downField("operationCtx").as[Context]
              variable <- c.downField("variable").as[Variable]
              resultVariable <- c.downField("resultVariable").as[Variable]
              keyValRef <- c.downField("keyValRef").as[Int]
            } yield {
              ReadVar(variable,keyValRef,resultVariable,operationContext).asInstanceOf[Event]
            }
          case "EventSendVarId" =>
            for {
              operationContext <- c.downField("operationCtx").as[Context]
              variableLookUp <- c.downField("variableLookUp").as[Variable]
            } yield {
              EventSendVarId(operationCtx = operationContext, variableLookUp = variableLookUp).asInstanceOf[Event]
            }
          case "" => Left(DecodingFailure("",c.history))
        }
        event
      }

      res match {
        case Left(value) => Left(DecodingFailure(value.reason,c.history))
        case Right(value) => value match {
          case Left(value) => Left(DecodingFailure(value.reason,c.history))
          case Right(value) => Right(value)
        }
      }
    }
  }

  val source = scala.io.Source.fromFile("src/main/resources/ia.json")

  val jsonContent = try {
    source.mkString
  } finally {
    source.close()
  }

  val parseResult = parse(jsonContent)

  if(parseResult.isLeft){
    println(s"Parsing failed with: $parseResult")
  }

  for {
    p <- parseResult
  } yield {
    val form = p.as[TopLevelSingleGraph[GraphMetaData,Unit,Event]]
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
            case s: SimpleEdge[Event] =>
              val label: String = s.metadata match {
                case Some(value) =>
                  value match {
                    case EventSendVarId(operationCtx, variableLookUp) =>
                      Table.empty
                        .appendRow(Row(List("type",s"${EventSendVarId.getClass.toString}")))
                        .appendRow(Row(List("ia-name",s"${s.label.getOrElse("")}?")))
                        .appendRow(Row(List("lookup-var",s"${variableLookUp.name} sId:${variableLookUp.ctx.spawnId} tId:${variableLookUp.ctx.transactionId}")))
                        .appendRow(Row(List("op-ctx",s"${operationCtx.transactionId},${operationCtx.spawnId}")))
                        .toHTML
                    case ReadVar(variable, keyValRef, resultVariable, operationCtx) =>
                      Table.empty
                        .appendRow(Row(List("type",s"${ReadVar.getClass.toString}")))
                        .appendRow(Row(List("ia-name",s"${s.label.getOrElse("")};")))
                        .appendRow(Row(List("variable", s"(${variable.name},${variable.ctx.transactionId},${variable.ctx.spawnId})")))
                        .appendRow(Row(List("keyValStore-ID", s"$keyValRef")))
                        .appendRow(Row(List("result-var",s"${resultVariable.name}, sId:${resultVariable.ctx.spawnId}, tId:${resultVariable.ctx.transactionId}")))
                        .appendRow(Row(List("op-ctx",s"${operationCtx.transactionId},${operationCtx.spawnId}")))
                        .toHTML
                    case ReceiveEventId(variable, operationCtx) =>
                        Table.empty
                          .appendRow(Row(List("type","ReceiveEventId")))
                          .appendRow(Row(List("ia-name",s"${s.label.getOrElse("")}?")))
                          .appendRow(Row(List("variable", s"(${variable.name},${variable.ctx.transactionId},${variable.ctx.spawnId})")))
                          .appendRow(Row(List("op-ctx", s"${operationCtx.transactionId},${operationCtx.spawnId}")))
                          .toHTML
                    case _ => ""
                  }
                case None => ""}
              Some(
                (dotRoot,
                  DotEdgeStmt(
                    NodeId(source.toOuter.jsonkey),
                    NodeId(target.toOuter.jsonkey),
                    List(DotAttr(Id("label"),Id(label)))
                  )))
        }
      }}

      val ctx = RepresentationCtx[Node[Unit],LDiEdge](
        dotRoot = dotRoot,
        edgeTransformer= edgeTransformer
      )

      println(DotRepresentation(f,ctx))
    }
  }
}
