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

import com.github.tizuck.jsonGraphSchema.TopLevelSingleGraph
import io.circe.parser

object Main extends App {
  val json =
    """
      |{
      |      "graph":{
      |        "metadata":{"meta":0},
      |        "nodes":{
      |          "q1":{
      |            "label":"1"
      |          }
      |        },
      |        "edges":[
      |          {
      |            "source":"q1",
      |            "target":"q2",
      |           "label":"foo"
      |          }
      |        ]
      |      }
      |    }
      |""".stripMargin

  import io.circe.generic.auto._

  sealed case class MetaData(meta: Int)

  val parsed = parser.parse(json)
  for { p <- parsed } yield {
    for { tpe <- p.as[TopLevelSingleGraph[MetaData, Unit, Unit]] } yield {
    }
  }
}
