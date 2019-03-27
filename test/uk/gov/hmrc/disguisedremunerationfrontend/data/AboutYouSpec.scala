/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.disguisedremunerationfrontend.data

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import ltbs.uniform._
import interpreters.logictable._
import cats.implicits._
import play.api.libs.json.Json
import uk.gov.hmrc.disguisedremunerationfrontend.Path

class AboutYouSpec extends WordSpec with Matchers {

  import AboutYou._
  import uk.gov.hmrc.disguisedremunerationfrontend.controllers.EmploymentStatus

  val validNino = "MY123456A"
  val invalidNino = "nino"
  val validUTR = "1234567890"
  val invalidUTR = "utr"

  "'About You' journey" should {

    "have a deceased date if and only if a person is not alive" in {

      val allJourneys = program[FxAppend[Stack, LogicTableStack]](None)
        .evalState(UniformCore())
        .giveExamples{
          case "aboutyou-completedby" => List(true, false)
          case "aboutyou-personalive" => List(true)
          case "aboutyou-deceasedbefore" => List(false)
          case _ => List(true,false)
        }
        .giveExamples(List("a","b","c"))
        .giveExamples(List(()))
        .giveExamples(List(Left("nino"),Right("utr"),Left(validNino),Right(validUTR)) : List[Either[String,String]])
        .giveExamples(EmploymentStatus.values.toList)
        .runEither
        .runWriter
        .runList
        .run

      allJourneys.zipWithIndex map {
        case ((outcome,Path(path)),i) =>
          println(s"SCENARIO ${i + 1}")
          println(path)
          println(outcome match {
            case Left(validationErrorMessage) =>
              s"  VALIDATION ERROR: $validationErrorMessage"
            case Right(Left(error: AboutYou.Error)) =>
              s"  OUTPUT ERROR: $error"
            case Right(Right(Some(aboutYou@AboutYou(_,alive,_,deceasedOn,_,_)))) =>
              // n.b. these foos test the implicit conversions
              // TODO do some assertions on converting too and from json (see sdil)
//              val foo = Json.toJson[AboutYou](aboutYou)
//              val bar = Json.toJson[Option[Option[AboutYou]]](aboutYou.some.some)
//              val foobar = Json.toJson[Option[Option[AboutYou]]](None.some)
//              val barfoo = Json.toJson[Option[Option[AboutYou]]](None)
              alive shouldBe (deceasedOn.isEmpty)
              s"  SUCCESS: $aboutYou"
            case Right(Right(None)) =>
              s"  SUCCESS: {none}"
          })
          println()
      }

    }

    "not need to complete if they were dead" in {

      val allJourneys = program[FxAppend[Stack, LogicTableStack]](None)
        .evalState(UniformCore())
        .giveExamples{
          case "aboutyou-completedby" => List(true, false)
          case "aboutyou-personalive" => List(false)
          case "aboutyou-deceasedbefore" => List(true)
        }
        .giveExamples(List("a","b","c"))
        .giveExamples(List(()))
        .giveExamples(List(Left(validNino),Right(validUTR)) : List[Either[String,String]])
        .giveExamples(EmploymentStatus.values.filter(x => x == EmploymentStatus.Employed).toList)
        .runEither
        .runWriter
        .runList
        .run

      allJourneys.zipWithIndex map {
        case ((outcome,Path(path)),i) =>
          println(s"SCENARIO ${i + 1}")
          println(path)
          println(outcome)
          println(outcome match {
            case Right(Left(NoNeedToComplete)) =>
              s"  SUCCESS: no need to complete"
            case Right(Right(None)) =>
              s"  SUCCESS: {none}"
            case _ => 1 shouldBe 2
          })
          println()
      }
    }
  }
}
