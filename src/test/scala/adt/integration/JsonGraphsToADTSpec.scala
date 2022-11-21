/*
 * Copyright 2022 Tilman Zuckmantel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.tizuck
package adt.integration

import jsonGraphSchema.{
  Graph,
  Node,
  Nodes,
  SimpleEdge,
  SimpleGraph,
  TopLevelMultipleGraphs
}

import io.circe.{Decoder, HCursor, parser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonGraphsToADTSpec extends AnyWordSpec with Matchers {
  "A JSON file containing multiple graphs" should {
    "when empty be successfully parsed and decoded into an according ADT" in {
      val path = "src/test/resources/emptyMulti.json"

      val source = scala.io.Source.fromFile(path)
      val jsonContent =
        try {
          source.mkString
        } finally {
          source.close()
        }

      val parsedJSON = parser.parse(jsonContent)

      parsedJSON should be(Symbol("right"))

      for { p <- parsedJSON } yield {
        for {
          decoded <- p.as[TopLevelMultipleGraphs[Unit, Unit, Unit]]
        } yield {
          decoded should equal(
            TopLevelMultipleGraphs[Unit, Unit, Unit](
              List.empty[Graph[Unit, Unit, Unit]]
            )
          )
        }
      }
    }
    "when loaded from resources/completemulti.json be successfully parsed and decoded into an according ADT" in {
      val path = "src/test/resources/completemulti.json"

      val source = scala.io.Source.fromFile(path)
      val jsonContent =
        try {
          source.mkString
        } finally {
          source.close()
        }

      val parsedJSON = parser.parse(jsonContent)

      parsedJSON should be(Symbol("right"))

      sealed case class MetaData(
          userdefined: String,
          tpe: Option[String] = None
      )

      implicit val metaDataDecoder: Decoder[MetaData] = { (c: HCursor) =>
        for {
          tpe <- c.downField("type").as[Option[String]]
          userdefined <- c.downField("user-defined").as[String]
        } yield {
          MetaData(userdefined = userdefined, tpe = tpe)
        }
      }

      parsedJSON should be(Symbol("right"))

      for { p <- parsedJSON } yield {
        for {
          decoded <- p.as[TopLevelMultipleGraphs[MetaData, MetaData, MetaData]]
        } yield {
          decoded should equal(
            TopLevelMultipleGraphs(
              List(
                SimpleGraph[MetaData, MetaData, MetaData](
                  directed = true,
                  tpe = Some("graph type"),
                  label = Some("graph label"),
                  metadata = Some(MetaData("values")),
                  nodes = Nodes(
                    List(
                      Node[MetaData](
                        Some("node label(0)"),
                        Some(MetaData("values", Some("node type"))),
                        "0"
                      ),
                      Node[MetaData](
                        Some("node label(1)"),
                        Some(MetaData("values", Some("node type"))),
                        "1"
                      )
                    )
                  ),
                  edges = List(
                    SimpleEdge[MetaData](
                      "0",
                      "1",
                      None,
                      Some("edge relationship"),
                      directed = true,
                      Some("edge label"),
                      Some(MetaData("values"))
                    )
                  )
                ),
                SimpleGraph[MetaData, MetaData, MetaData](
                  directed = true,
                  tpe = Some("graph type"),
                  label = Some("graph label"),
                  metadata = Some(MetaData("values")),
                  nodes = Nodes(
                    List(
                      Node[MetaData](
                        Some("node label(0)"),
                        Some(MetaData("values")),
                        "0"
                      ),
                      Node[MetaData](
                        Some("node label(1)"),
                        Some(MetaData("values")),
                        "1"
                      )
                    )
                  ),
                  edges = List(
                    SimpleEdge[MetaData](
                      "1",
                      "0",
                      None,
                      Some("edge relationship"),
                      directed = true,
                      Some("edge label"),
                      Some(MetaData("values"))
                    )
                  )
                )
              )
            )
          )
        }
      }
    }
  }
}
