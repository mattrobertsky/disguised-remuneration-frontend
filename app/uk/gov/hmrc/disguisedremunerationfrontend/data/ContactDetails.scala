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

  lazy val nameRegex = """^[a-zA-Z0-9',-./ ]*$"""
  lazy val townCountyRegex = """^[a-zA-Z0-9',-./ ]*$"""
  lazy val telephoneRegex = """^\+?[0-9\s\(\)]{1,20}$"""
  lazy val emailRegex = """^[a-zA-Z0-9\./_-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"""
  lazy val postCodeRegex = """([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\s?[0-9][A-Za-z]{2})"""

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
      address <- ask[Address]("confirm-contact-details")
        .defaultOpt(default.map(_.address))
        .validating(
          "street-limit",
          address => address.line1.length <= 40
        )
        .validating(
          "street-format",
          address => address.line1.matches(nameRegex)
        )
        .validating(
          "town-limit",
          address => address.town.length <= 40
        )
        .validating(
          "town-format",
          address => address.town.matches(townCountyRegex)
        )
        .validating(
          "county-limit",
          address => address.county.getOrElse("").length <= 40
        )
        .validating(
          "county-format",
          address => address.county.getOrElse("").matches(townCountyRegex)
        )
        .validating(
          "postcode-limit",
          address => address.postcode.length <= 40
        )
        .validating(
          "postcode-format",
          address => address.postcode.matches(postCodeRegex)
        )
      telAndEmail <- ask[TelAndEmail]("confirm-contact-prefs")
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
        .validating(
          "email-or-phone-number-needed",
          {case TelAndEmail(None,None) => false; case _ => true}
        )
      
    } yield {
      ContactDetails(address, telAndEmail)
    }
  }
}
