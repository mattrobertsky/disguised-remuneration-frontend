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

import ltbs.uniform.{UniformCore, _}
import ltbs.uniform.interpreters.logictable.{LogicTableStack, _}
import org.atnos.eff.FxAppend
import org.atnos.eff.syntax.all._
import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.disguisedremunerationfrontend.data.ContactDetails._
import cats.implicits._
import uk.gov.hmrc.disguisedremunerationfrontend.Path

class ContactDetailsSpec extends WordSpec with Matchers {

  val address = Address("a","b".some,"c","d".some,"AA1 1AA")
  val telAndEmail = TelAndEmail("07700 900 982".some,"foo@bar.com".some) // TODO test invalid
  val details = ContactDetails(address, telAndEmail)

  "'Contact Details' journey" should {
    "create a ContactDetails instance" in {

      val allJourneys = program[FxAppend[Stack, LogicTableStack]](None)
        .evalState(UniformCore())
        .giveExamples(List(address) )
        .giveExamples(List(telAndEmail))
        .runEither
        .runWriter
        .runList
        .run


      allJourneys.zipWithIndex map {
        case ((outcome,Path(path)),i) =>
          println(s"SCENARIO ${i + 1}")
          println(path)
          println(outcome match {
            case Right(contactDetails@ContactDetails(address,telephoneAndEmail)) =>
                contactDetails shouldBe details
              s"  SUCCESS: $contactDetails"
          })
          println()
      }
    }
  }

}
