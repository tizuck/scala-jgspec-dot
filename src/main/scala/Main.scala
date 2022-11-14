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
      s"<table>${rows.map(_.toHTML).mkString}</table>"
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
            case s:SimpleEdge[Event] =>
              val label = s"${s.label.getOrElse("")}${
                s.metadata match {
                  case Some(value) => s"(${
                    value match {
                      case EventSendVarId(operationCtx, variableLookUp) =>
                        s"${variableLookUp.name} sId:${variableLookUp.ctx.spawnId} tId:${variableLookUp.ctx.transactionId}!"
                      case ReadVar(variable, keyValRef, resultVariable, operationCtx) =>
                        s"lookup:(${variable.name}, sId:${variable.ctx.spawnId}, tId:${variable.ctx.transactionId}) " +
                          s"in DB:$keyValRef result: (${resultVariable.name}, sId:${resultVariable.ctx.spawnId}, tId:${resultVariable.ctx.transactionId});"
                      case ReceiveEventId(variable, operationCtx) => val table = Table.empty
                        val tableWithName = table.appendRow(Row(List("<b>ReceiveEventId</b>")))
                        val tableWithVariable = tableWithName.appendRow(Row(List("variable", s"(${variable.name},${variable.ctx.transactionId},${variable.ctx.spawnId})")))
                        val tableWithOperationContext = tableWithVariable.appendRow(Row(List("op-ctx", s"${operationCtx.transactionId},${operationCtx.spawnId}")))
                        tableWithOperationContext.toHTML
                      case _ => ""
                    }})"
                  case None => ""
                }}"
              Some(
                (dotRoot,
                  DotEdgeStmt(
                    NodeId(source.toOuter.jsonkey),
                    NodeId(target.toOuter.jsonkey),
                    List(DotAttr(Id("label"),Id(label)))
                  )))
          }
        }
      }

      val ctx = RepresentationCtx[Node[Unit],LDiEdge](
        dotRoot = dotRoot,
        edgeTransformer= edgeTransformer
      )

      println(DotRepresentation(f,ctx))
    }
  }
}
