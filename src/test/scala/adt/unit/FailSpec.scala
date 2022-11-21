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
package adt.unit

import jsonGraphSchema.Nodes

import io.circe.parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FailSpec extends AnyWordSpec with Matchers {
  "A JSON file that is not specified according to the JSON graph specification v2" when {
    "a specification deviation in a node field with a wrong type for a label" should {
      "calculate a DecodingFailure" in {
        val jsonErr =
          """
            |{
            |  "q1":{"label":1},
            |  "q2":{}
            |}
            |""".stripMargin

        val parseResCon = parser.parse(jsonErr)
        for{parseRes <- parseResCon} yield {
          val decodedCon = parseRes.as[Nodes[Unit]]
          decodedCon should be(Symbol("left"))
        }
      }
    }
  }
}
