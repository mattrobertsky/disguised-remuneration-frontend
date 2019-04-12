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
import play.api.i18n.Messages

case class TelAndEmail(telephone: Option[String], email: Option[String])

case class ContactDetails(
  address: Address,
  telephoneAndEmail: TelAndEmail
)

object ContactDetails {

  lazy val nameRegex = """^[a-zA-Z0-9'@,-./ ]*$"""
  lazy val telephoneRegex = """^[0-9+ ]*$"""
  lazy val emailRegex = """^[a-z0-9.-_@]*$"""

  type Stack = Fx.fx2[
    UniformAsk[Address, ?],
    UniformAsk[TelAndEmail, ?]
  ]
  def program[R
      : _uniformCore
      : _uniformAsk[Address, ?]
      : _uniformAsk[TelAndEmail, ?]
  ](default: Option[ContactDetails]): Eff[R, ContactDetails] = {
    for {
      address <- ask[Address]("contactdetails-address")
        .defaultOpt(default.map(_.address))
        .validating(
          "street-limit",
          address =>  address.line1.length <= 40
        )
        .validating(
          "street-format",
          address => address.line1.matches(nameRegex)
        )
      telAndEmail <- ask[TelAndEmail]("contactdetails-telehoneemail")
        .defaultOpt(default.map(_.telephoneAndEmail))
        .validating(
          "phone-limit",
          contact =>  contact.telephone.fold(true)(_.length <= 24)
        )
        .validating(
          "phone-format",
          contact => contact.telephone.fold(true)(_.matches(telephoneRegex))
        )
        .validating(
          "email-limit",
          contact => contact.email.fold(true)(_.length <= 256)
        )
        .validating(
          "email-format",
          contact => contact.email.fold(true)(_.matches(emailRegex))
        )
    } yield {
      ContactDetails(address, telAndEmail)
    }
  }
}
