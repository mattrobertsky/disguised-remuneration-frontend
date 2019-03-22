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
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class TelAndEmail(telephone: Option[String], email: Option[String])

object TelAndEmail {
  implicit val TelAndEmailFormatter: Format[TelAndEmail] = Json.format[TelAndEmail]
}


case class ContactDetails(
  address: Address,
  telephoneAndEmail: TelAndEmail
)

object ContactDetails {

  implicit val contactDetailsFormatter: Format[ContactDetails] = Json.format[ContactDetails]

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
  ]: Eff[R, ContactDetails] = {
    for {
      address <- ask[Address]("contactdetails-address")
        .validating(
          "Enter the building or street of your address",
          address => !address.line1.isEmpty()
        )
        .validating(
          "Building or street must be 40 characters or less",
          address =>  address.line1.length <= 40
        )
        .validating(
          "Building or street must only include letters a to z, numbers, apostrophes, ampersands, commas, hyphens, full stops, forward slashes and spaces",
          address => address.line1.matches(nameRegex)
        )
      telAndEmail <- ask[TelAndEmail]("contactdetails-telehoneemail")
        .validating(
          "Telephone number must be 24 characters or less",
          contact =>  contact.telephone.fold(true)(_.length <= 24)
        )
        .validating(
          "Enter a telephone number, like 01632 960 001, 07700 900 982 or +44 0808 157 0192",
          contact => contact.telephone.fold(true)(_.matches(telephoneRegex))
        )
        .validating(
          "Email address must be 256 characters or less",
          contact => contact.email.fold(true)(_.length <= 256)
        )
        .validating(
          "Email address must only contain letters a to z, numbers, full stops, hyphens, underscores and @ signs",
          contact => contact.email.fold(true)(_.matches(emailRegex))
        )
    } yield {
      ContactDetails(address, telAndEmail)
    }
  }
}
