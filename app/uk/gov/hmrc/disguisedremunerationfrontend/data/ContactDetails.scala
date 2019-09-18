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

import cats.data.NonEmptyList
import cats.implicits._
import ltbs.uniform.{::, Language, NilTypes, _}
import play.api.i18n.{Messages => _}

import scala.language.higherKinds


case class TelAndEmail(telephone: String, email: String)

case class ContactDetails(
  address: Address,
  telephoneAndEmail: TelAndEmail
)

object ContactDetails {
  type TellTypes = NilTypes
  type AskTypes = Address :: TelAndEmail :: NilTypes

  lazy val nameRegex = """^[a-zA-Z0-9',-./ ]*$"""
  lazy val townCountyRegex = """^[a-zA-Z0-9',-./ ]*$"""
  lazy val telephoneRegex = """^\+?[0-9\s\(\)]{1,20}$|.{0}"""
  lazy val emailRegex = """^[a-zA-Z0-9\./_-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$|.{0}"""
  lazy val postCodeRegex = """([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\s?[0-9][A-Za-z]{2}|.{0})"""

  def contactDetailsProgram[F[_] : cats.Monad](
    interpreter: Language[F, TellTypes, AskTypes]
  ): F[ContactDetails] = {
    import interpreter._

    for {
      address <- ask[Address]("confirm-contact-details")
      telAndEmail <- ask[TelAndEmail]("confirm-contact-prefs", validation =
        List(
          List(
            Rule.fromPred(
              telAndEmail => telAndEmail.telephone.length <= 24,
              (ErrorMsg("phone-limit"), NonEmptyList.one(List("telephone")))
            ),
            Rule.fromPred(
              telAndEmail => telAndEmail.telephone.matches(telephoneRegex),
              (ErrorMsg("phone-format"), NonEmptyList.one(List("telephone")))
            ),
            Rule.fromPred(
              telAndEmail => telAndEmail.email.length <= 256,
              (ErrorMsg("email-limit"), NonEmptyList.one(List("email")))
            ),
            Rule.fromPred(
              telAndEmail => telAndEmail.email.matches(emailRegex),
              (ErrorMsg("email-format"), NonEmptyList.one(List("email")))
            ),
            Rule.fromPred(
              {
                case a => a.email.matches(nonEmptyStringRegex) || a.telephone.matches(nonEmptyStringRegex)
                case _ => false
              },
              (ErrorMsg("email-or-phone-number-needed"), NonEmptyList[InputPath](List("email"), List(List("telephone"))))
            )
          )
        )
      )
    } yield {
      ContactDetails(address, telAndEmail)
    }
  }
}
