package com.github.tizuck
package adt.integration

import dot.DotRepresentation
import dot.DotRepresentation.RepresentationCtx
import jsonGraphSchema.{Node, SimpleEdge, TopLevelSingleGraph}

import io.circe._
import io.circe.parser.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot.{
  DotAttr,
  DotEdgeStmt,
  DotGraph,
  DotRootGraph,
  Id,
  NodeId
}

import scala.io.BufferedSource

/** Tests the parsing and Dot Representation of a Single Graph containing 3
  * nodes and edges between these note with metadata of different type with a
  * common superclass.
  *
  * This test shows exemplary how to define an AST for the metadata types and
  * construct a custom decoder for metadata json content and how to use this
  * decoder when parsing the json file. Further, an edgeTransformer is
  * constructed that shows how to customly transform edges into a desired
  * printed form containing HTML like strings.
  */
class InterfaceAutomatonSpec extends RefSpec with Matchers {
  object `testing the parsing and dot representation of an interface automaton` {

    /** Abstract Syntax Tree of Metadata contained in a graph representation of
      * an interface automaton.
      */
    object ast {
      trait Event {
        val operationCtx: Context
      }

      sealed case class GraphContext(
          spawnId: Int,
          transactionId: Int,
          keyValRef: List[Int]
      )

      sealed case class GraphMetaData(ctx: GraphContext)

      sealed case class Context(spawnId: Int, transactionId: Int)

      sealed case class Variable(name: String, ctx: Context)

      sealed case class ReceiveEventId(
          variable: Variable,
          override val operationCtx: Context
      ) extends Event

      sealed case class ReadVar(
          variable: Variable,
          keyValRef: Int,
          resultVariable: Variable,
          override val operationCtx: Context
      ) extends Event

      sealed case class EventSendVarId(
          override val operationCtx: Context,
          variableLookUp: Variable
      ) extends Event

    }

    /** Abstract Syntax Tree of HTML Table expressions with the possibility to
      * create a string from it using the `toHTML` field.
      */
    object helper {
      sealed case class Row(cells: List[String]) {
        def toHTML: String = {
          s"<tr>${cells.map(c => s"<td>$c</td>").mkString}</tr>"
        }

        def appendCell(cell: String): Row =
          this.copy(cells = cells.appended(cell))
      }

      sealed case class Table(rows: List[Row]) {
        def toHTML: String = {
          s"<<table>${rows.map(_.toHTML).mkString}</table>>"
        }
        def appendRow(row: Row): Table = {
          this.copy(rows = rows.appended(row))
        }
      }

      final object Table {
        def empty: Table = Table(Nil)
      }
    }

    import ast._
    import io.circe.generic.auto._

    /** Implementation of a JSON Decoder for subclasses of [[ast.Event]]. This
      * decoder can not be auto generated since we want to differentiate the
      * type of the [[ast.Event]] subclass by a field lookup on `tpe`.
      *
      * Notice that for all other classes of [[ast]] the Circe framework is able
      * to auto-generate [[Decoder]] instances.
      *
      * For more information on creating decoders see the webpage of
      * [[https://circe.github.io/circe/codec.html Circe]].
      */
    implicit val metaEventDecoder: Decoder[Event] = { (c: HCursor) =>
      {
        val res = for {
          tpe <- c.downField("tpe").as[String]
        } yield {
          val event = tpe match {
            case "ReceiveEventID" =>
              for {
                operationContext <- c.downField("operationCtx").as[Context]
                variable <- c.downField("variable").as[Variable]
              } yield {
                ReceiveEventId(variable, operationContext).asInstanceOf[Event]
              }
            case "ReadWithVar" =>
              for {
                operationContext <- c.downField("operationCtx").as[Context]
                variable <- c.downField("variable").as[Variable]
                resultVariable <- c.downField("resultVariable").as[Variable]
                keyValRef <- c.downField("keyValRef").as[Int]
              } yield {
                ReadVar(variable, keyValRef, resultVariable, operationContext)
                  .asInstanceOf[Event]
              }
            case "EventSendVarId" =>
              for {
                operationContext <- c.downField("operationCtx").as[Context]
                variableLookUp <- c.downField("variableLookUp").as[Variable]
              } yield {
                EventSendVarId(
                  operationCtx = operationContext,
                  variableLookUp = variableLookUp
                )
                  .asInstanceOf[Event]
              }
            case "" => Left(DecodingFailure("", c.history))
          }
          event
        }

        res match {
          case Left(value) => Left(DecodingFailure(value.reason, c.history))
          case Right(value) =>
            value match {
              case Left(value) => Left(DecodingFailure(value.reason, c.history))
              case Right(value) => Right(value)
            }
        }
      }
    }

    // Opening and parsing the file at specified location
    val source: BufferedSource =
      scala.io.Source.fromFile("src/test/resources/ia.json")

    val jsonContent: String =
      try {
        source.mkString
      } finally {
        source.close()
      }

    val parseResult: Either[ParsingFailure, Json] = parse(jsonContent)

    // Checking if parsing was successful
    parseResult should be(Symbol("right"))

    for {
      json <- parseResult
    } yield {
      // trying to Decode the parsed json as an instance of
      // a single graph with the according Metadata classes.
      val jsonDecodingRes =
        json.as[TopLevelSingleGraph[GraphMetaData, Unit, Event]]

      jsonDecodingRes should be(Symbol("right"))

      for { topLevelGraphContainer <- jsonDecodingRes } yield {
        val graph = topLevelGraphContainer.graph

        val dotRoot =
          DotRootGraph(id = graph.id.map(i => Id(i)), directed = graph.directed)

        import helper._

        def varToCell(variable: ast.Variable): String = {
          s"(${variable.name},${variable.ctx.spawnId},${variable.ctx.transactionId})"
        }

        /** Example of an Edge transformer to programmatically define how edges
          * of a graph should be transformed into DOT statements.
          *
          * For a guide on how to create edge transformer in general see:
          * [[https://www.scala-graph.org/guides/dot.html]]
          *
          * @param innerEdge
          *   edge that is transformed by the transformer
          * @return
          *   optional tuple consisting of the graph that the transformed edge
          *   is attached to and the resulting dot edge in the form of a
          *   [[https://www.scala-graph.org/guides/dot.html DOT AST]]
          */
        def edgeTransformer(
            innerEdge: scalax.collection.Graph[Node[Unit], LDiEdge]#EdgeT
        ): Option[(DotGraph, DotEdgeStmt)] = {

          innerEdge.edge match {
            case LDiEdge(source, target, label) =>
              label match {
                case s: SimpleEdge[Event] =>
                  val label: String = s.metadata match {
                    case Some(event) =>
                      event match {
                        case ast.EventSendVarId(operationCtx, variableLookUp) =>
                          Table.empty
                            .appendRow(Row(List("type", "EventSendVarId")))
                            .appendRow(
                              Row(List("ia-name", s"${s.label.getOrElse("")}!"))
                            )
                            .appendRow(
                              Row(List("lookup-var", varToCell(variableLookUp)))
                            )
                            .appendRow(
                              Row(
                                List(
                                  "op-ctx",
                                  s"${operationCtx.transactionId}," +
                                    s"${operationCtx.spawnId}"
                                )
                              )
                            )
                            .toHTML
                        case ast.ReadVar(
                              variable,
                              keyValRef,
                              resultVariable,
                              operationCtx
                            ) =>
                          Table.empty
                            .appendRow(Row(List("type", "ReadVar")))
                            .appendRow(
                              Row(List("ia-name", s"${s.label.getOrElse("")};"))
                            )
                            .appendRow(
                              Row(List("variable", varToCell(variable)))
                            )
                            .appendRow(
                              Row(List("keyValStore-ID", s"$keyValRef"))
                            )
                            .appendRow(
                              Row(List("result-var", varToCell(resultVariable)))
                            )
                            .appendRow(
                              Row(
                                List(
                                  "op-ctx",
                                  s"${operationCtx.transactionId}," +
                                    s"${operationCtx.spawnId}"
                                )
                              )
                            )
                            .toHTML
                        case ast.ReceiveEventId(variable, operationCtx) =>
                          Table.empty
                            .appendRow(Row(List("type", "ReceiveEventId")))
                            .appendRow(
                              Row(List("ia-name", s"${s.label.getOrElse("")}?"))
                            )
                            .appendRow(
                              Row(List("variable", varToCell(variable)))
                            )
                            .appendRow(
                              Row(
                                List(
                                  "op-ctx",
                                  s"${operationCtx.transactionId}," +
                                    s"${operationCtx.spawnId}"
                                )
                              )
                            )
                            .toHTML
                        case _ => ""
                      }
                    case None => ""
                  }
                  Some(
                    (
                      dotRoot,
                      DotEdgeStmt(
                        NodeId(source.toOuter.jsonkey),
                        NodeId(target.toOuter.jsonkey),
                        List(DotAttr(Id("label"), Id(label)))
                      )
                    )
                  )
              }
          }
        }

        val ctx = RepresentationCtx[Node[Unit], LDiEdge](
          dotRoot = dotRoot,
          edgeTransformer = edgeTransformer
        )

        val dotRepresentation = DotRepresentation(topLevelGraphContainer, ctx)

        val q1_q2_table =
          """
              |<<table>
              | <tr><td>type</td><td>ReadVar</td></tr>
              | <tr><td>ia-name</td><td>read;</td></tr>
              | <tr><td>variable</td><td>(c,0,0)</td></tr>
              | <tr><td>keyValStore-ID</td><td>0</td></tr>
              | <tr><td>result-var</td><td>(res,0,0)</td></tr>
              | <tr><td>op-ctx</td><td>0,0</td></tr>
              |</table>>
              |""".stripMargin.filterNot(_.isWhitespace)

        val q0_q1_table =
          """
              |<<table>
              | <tr><td>type</td><td>ReceiveEventId</td></tr>
              | <tr><td>ia-name</td><td>req?</td></tr>
              | <tr><td>variable</td><td>(c,0,0)</td></tr>
              | <tr><td>op-ctx</td><td>0,0</td></tr>
              |</table>>
              |""".stripMargin.filterNot(_.isWhitespace)

        val q2_q0_table =
          """
              |<<table>
              | <tr><td>type</td><td>EventSendVarId</td></tr>
              | <tr><td>ia-name</td><td>snd!</td></tr>
              | <tr><td>lookup-var</td><td>(res,0,0)</td></tr>
              | <tr><td>op-ctx</td><td>0,0</td></tr>
              |</table>>
              |""".stripMargin.filterNot(_.isWhitespace)

        val expectedValueSorted =
          s"""
              |digraph {
              | q0 -> q1 [label = $q0_q1_table]
              | q1 -> q2 [label = $q1_q2_table]
              |	q2 -> q0 [label = $q2_q0_table]
              |}
              |""".stripMargin.filterNot(_.isWhitespace)

        val dotSortedWithoutWS = {
          val linesSeparated = dotRepresentation.dot.linesIterator.toList
          val linesSeparatedWithoutWS =
            linesSeparated.map(_.filterNot(_.isWhitespace))
          // We should have 5 lines consisting of the three graph edges
          // and two additional edges to syntactically open and close the graph
          linesSeparated should have(length(5))
          linesSeparatedWithoutWS.foreach(f => {
            f should (
              startWith("q0->q1") or
                startWith("q1->q2") or
                startWith("q2->q0") or
                startWith("digraph") or
                startWith("}")
            )
          })

          def findFirstNumber(s1: String) = {
            """\d+""".r.findFirstIn(s1).map(_.toInt).get
          }

          val sortedLines =
            linesSeparatedWithoutWS.sorted((s1: String, s2: String) => {
              (s1, s2) match {
                case ("digraph{", _) => -1
                case (_, "digraph{") => 1
                case ("}", _)        => 1
                case (_, "}")        => -1
                case (s1, s2) =>
                  val firstNumbers1 = findFirstNumber(s1)
                  val firstNumbers2 = findFirstNumber(s2)
                  firstNumbers1.compare(firstNumbers2)
              }
            })
          sortedLines.mkString
        }

        val expectedValueSortedWithoutWS =
          expectedValueSorted.filterNot(_.isWhitespace)

        expectedValueSortedWithoutWS should equal(dotSortedWithoutWS)

      }
    }
  }
}
