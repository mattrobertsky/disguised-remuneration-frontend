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

import ltbs.uniform._
import org.atnos.eff.{Eff, Fx}
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoDoNotKnow._
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoDoNotKnow.z.DoNotKnow
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.{EmploymentStatus, YesNoDoNotKnow}
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration.Date


case class Scheme(
  name: String,
  dotasReferenceNumber: Option[String],
  caseReferenceNumber: Option[String],
  schemeStart: Option[Date],
  schemeStopped: Option[Date],
  employee: Option[Employer],
  loanRecipient: Boolean,
  loanRecipientName: Option[String],
  settlement: Option[TaxSettlement]
)

import play.api.libs.json.{Format, Json}

object Scheme {

  implicit val schemeFormatter: Format[Scheme] = Json.format[Scheme]

  lazy val nameRegex = """^[a-zA-Z0-9'@,-./() ]*$"""
  lazy val caseRefRegex = """^[a-zA-Z0-9-]*$"""
  lazy val payeRegex = """^\d{3}/[A-Za-z]{2}\d{3}$"""
  lazy val maxNameLength = 50


  val earliestDate = LocalDate.parse("1999-04-05")

  def isInRange(d: LocalDate) = d.isAfter(earliestDate) && d.isBefore(LocalDate.now())

  def startBeforeEnd(dates: (LocalDate, LocalDate)): Boolean = (dates._1, dates._2) match {
    case (start, end) if (isInRange(start) && isInRange(end) && start.isBefore(end)) => true
    case _ => false
  }

  type Stack = Fx.fx8[
    UniformAsk[String,?],
    UniformAsk[Option[String],?],
    UniformAsk[Boolean,?],
    UniformAsk[TaxSettlement,?],
    UniformAsk[Option[Employer],?],
    UniformAsk[YesNoDoNotKnow,?],
    UniformAsk[Date,?],
    UniformAsk[(Date, Date),?]
    ]

  def program[R
    : _uniformCore
    : _uniformAsk[String,?]
    : _uniformAsk[Option[String],?]
    : _uniformAsk[TaxSettlement,?]
    : _uniformAsk[Boolean,?]
    : _uniformAsk[Option[Employer],?]
    : _uniformAsk[YesNoDoNotKnow,?]
    : _uniformAsk[(Date,Date),?]
    : _uniformAsk[Date,?]
  ]: Eff[R, Option[Scheme]] =
    for {
      schemeName            <-  ask[String]("scheme-name")
                                    .validating(
                                      "Enter the name of the scheme",
                                      name => !name.isEmpty
                                    )
                                  .validating(
                                    "Scheme name must be 100 characters or less",
                                    name => name.length < 100
                                  )
                                  .validating(
                                    "Scheme name must only include letters a to z, numbers, apostrophes, ampersands, commas, hyphens, full stops, forward slashes, round brackets and spaces",
                                    name => name.matches(nameRegex)
                                  )
      dotasNumber           <-  ask[YesNoDoNotKnow]("scheme-dotas")
                                  .validating(
                                    "Disclosure of Tax Avoidance Schemes (DOTAS) number must be 8 numbers",
//                                  case name => println(s"DOTAS LENGTH = ${name.toString}"); name.entryName.length() == 8  // Need to get actual value!
                                    yn  => true
                                  )
      schemeReferenceNumber <-  ask[Option[String]]("scheme-refnumber")
                                  .validating(
                                    "HMRC case reference number must be 10 characters or less",
                                    _ match {
                                      case Some(ref) => ref.length() <= 10
                                      case _ => true
                                    }
                                  )
                                  .validating(
                                    "HMRC case reference number must only include letters a to z, numbers and hyphens",
                                    _ match {
                                      case Some(ref) => println(s"*REF: $ref"); ref.matches(caseRefRegex)
                                      case _ => true
                                    }
                                  )
      stillUsingScheme      <-  ask[Boolean]("scheme-stillusing")
      stillUsingYes         <-  ask[Date]("scheme-stillusingyes")
                                  .validating(s"The date you started using the scheme must after $earliestDate", isInRange(_))
                                  .validating("The date you started using the scheme must be in the past", _.isBefore(LocalDate.now()))
                                  .in[R] when stillUsingScheme
      stillUsingNo          <-  ask[(Date, Date)]("scheme-stillusingno")
                                  .validating("The date you stopped using the scheme must be the same as or after the date you started using the scheme", startBeforeEnd _)
                                  .in[R] when !stillUsingScheme
      employer              <-  ask[Option[Employer]]("scheme-employee")
                                    .validating(
                                      "Enter the employer's name",
                                      _ match {
                                        case Some(employer) =>  println(employer); !employer.name.isEmpty
                                        case _ => true
                                      }
                                    )
                                    .validating(
                                      "Employer's name must be 50 characters or less",
                                      _ match {
                                        case Some(employer) => employer.name.length <= maxNameLength
                                        case _ => true
                                      }
                                    )
                                    .validating(
                                      "Employer's name must only include letters a to z, numbers, apostrophes, ampersands, commas, hyphens, full stops, forward slashes, round brackets and spaces",
                                      _ match {
                                        case Some(employer) =>  employer.name.matches(nameRegex)
                                        case _ => true
                                      }
                                    )
                                    .validating(
                                      "Enter the employer PAYE reference in the correct format",
                                      _ match {
                                        case Some(employer) =>  employer.paye.matches(payeRegex)
                                        case _ => true
                                      }
                                    )
                                    .in[R]
      recipient             <-  ask[Option[String]]("scheme-recipient")
                                  .validating(
                                    "Enter the name of who the loan was made out to",
                                    _ match {
                                      case Some(name) =>  !name.isEmpty()
                                      case _ => true
                                    }
                                  )
                                  .validating(
                                    "Name of who the loan was made out to must be 50 characters or less",
                                    _ match {
                                      case Some(name) =>  name.length <= 50
                                      case _ => true
                                    }
                                  )
                                  .validating(
                                    "Name of who the loan was made out to must only include letters a to z, numbers, apostrophes, ampersands, commas, hyphens, full stops, forward slashes, round brackets and spaces",
                                    _ match {
                                      case Some(name) =>  name.matches(nameRegex)
                                      case _ => true
                                    }
                                  )
                                  .in[R]
      taxNIPaid             <-  ask[Boolean]("scheme-agreedpayment").in[R]
      settlementStatus      <-  ask[TaxSettlement]("scheme-settlementstatus")
                                  .validating(
                                     "Enter how much tax and National Insurance you have paid, or agreed with us to pay",
                                     settlement => settlement.amount > 0
                                  )
                                  .in[R] when taxNIPaid
    } yield {

      val dotas = dotasNumber match {
        case Yes(ref) => Some(ref)
        case No => Some("No")
        case DoNotKnow => Some("Do not know")
      }

      val(startDate, stopDate): (Option[LocalDate], Option[LocalDate]) =
                                  if (stillUsingScheme)
                                      (stillUsingYes, None)
                                  else
                                    stillUsingNo.map(period => (Some(period._1), Some(period._2)))
                                      .getOrElse((None,None))

      val scheme = Scheme(
        name = schemeName,
        dotasReferenceNumber = dotas,
        caseReferenceNumber = schemeReferenceNumber,
        schemeStart = startDate,
        schemeStopped = stopDate,
        employee = employer,
        loanRecipient = recipient.isEmpty,
        loanRecipientName = recipient,
        settlement = settlementStatus
      )
      Some(scheme)
    }

}
