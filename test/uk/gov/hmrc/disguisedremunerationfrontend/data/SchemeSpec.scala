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

import java.time.LocalDate

import ltbs.uniform.UniformCore
import ltbs.uniform.interpreters.logictable.LogicTableStack
import org.atnos.eff.FxAppend
import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.disguisedremunerationfrontend.data.Scheme._
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoDoNotKnow
import cats.implicits._
import ltbs.uniform.{UniformCore, _}
import org.atnos.eff.syntax.all._
import ltbs.uniform._
import interpreters.logictable._
import org.atnos.eff.syntax._
import uk.gov.hmrc.disguisedremunerationfrontend.Path
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoDoNotKnow.z.DoNotKnow
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoDoNotKnow.{No, Yes}

import scala.collection.immutable

class SchemeSpec  extends WordSpec with Matchers {

  val date: LocalDate = java.time.LocalDate.now.minusDays(1) // TODO validation message is wrong here, make now to see
  val employee = Employer("bob", "123/ab123")
  val taxSettlement = TaxSettlement(100, date)
  val scheme = Scheme("a", "b".some, "c".some, Scheme.earliestDate, date.some, employee.some, loanRecipient = true, "d".some, taxSettlement.some)
  val yesNoList: List[YesNoDoNotKnow] = List(Yes("y"), No, DoNotKnow)



  // TODO - note to self .giveExamples matches the types asked for in the for comprehension in Scheme.program

//  "'Scheme' journey" should {
//    "create a Scheme instance" in {
//
//      val allJourneys = program[FxAppend[Stack, LogicTableStack]](None)
//        .evalState(UniformCore())
//        .giveExamples(List("a","b","c") )
//        .giveExamples(List("a".some))
//        .giveExamples(List(true,false))
//        .giveExamples(List(employee.some))
//        .giveExamples(List(taxSettlement))
//        .giveExamples(List(date))
//        .giveExamples(List((date,date)))
//        .giveExamples(yesNoList)
//        .runEither
//        .runWriter
//        .runList
//        .run
//
//
//      allJourneys.zipWithIndex map {
//        case ((outcome,Path(path)),i) =>
//          println(s"SCENARIO ${i + 1}")
//          println(path)
//          println(outcome match {
//            case Right(nscheme) =>
////              nscheme shouldBe scheme
//              s"  SUCCESS: $scheme"
//          })
//          println()
//          1 shouldBe 1
//      }
//    }
//  }


}
