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

import ltbs.uniform._
import org.atnos.eff._
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration._

case class ContactDetails(
  address: Address,
  telephoneAndEmail: (Option[String], Option[String])
)

object ContactDetails {

  type Stack = Fx.fx2[
    UniformAsk[Address, ?],
    UniformAsk[(String, String), ?]
  ]

  def program[R
      : _uniformCore
      : _uniformAsk[Address, ?]
      : _uniformAsk[(String, String), ?]
  ]: Eff[R, ContactDetails] = {
    for {
      address <- ask[Address]("contactdetails-address")
                 // .validating("Address incorrect", x => (x.line1.length < 35) &&  (x.line2.length < 35))
      telAndEmail <- ask[(String, String)]("contactdetails-telehoneemail")
                  //("Please verify telephone/email")
                 //   .validating(" tel & email incorrect", te => te._1.length < 20 && te._2.length < 35)
    } yield {
      println(address)
      println(telAndEmail)
      ContactDetails(address, (Some(telAndEmail._1), Some(telAndEmail._2)))
    }
  }
}