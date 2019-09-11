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

//import org.scalatest.{Matchers, WordSpec}
//import org.scalatestplus.play.guice.GuiceOneAppPerSuite
//import play.api.http.Status
//import play.api.libs.json._
//
//import JsonConversion._
//
//class JsonConversionSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {
//
//  "eitherFormatter" should {
//    "encode and decode correctly" in {
//      import JourneyState._
//      val ef = eitherFormatter[String,String]("nino", "utr")
//
//      val inputs = List(Left("one"),Right("two"))
//      inputs.map { input =>
//        ef.reads(ef.writes(input)) shouldBe JsSuccess(input)
//      }
//    }
//  }
//}
