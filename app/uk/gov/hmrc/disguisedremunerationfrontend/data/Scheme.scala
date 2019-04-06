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
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.{EmploymentStatus, YesNoDoNotKnow}
import cats.implicits._
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoDoNotKnow.Yes

case class Scheme(
  name: String,
  dotasReferenceNumber: Option[String],
  caseReferenceNumber: Option[String],
  schemeStart: Date,
  schemeStopped: Option[Date],
  employee: Option[Employer],
  loanRecipient: Boolean,
  loanRecipientName: Option[String],
  settlement: Option[TaxSettlement],
  loanDetailsProvided: Map[Year, LoanDetails] = Map.empty
) {
  lazy val loanDetails: Map[Year, Option[LoanDetails]] = {
    val cutoffDate = LocalDate.of(1999,5,1).financialYear
    val start = Math.max(schemeStart.financialYear, cutoffDate)
    val years = start to schemeStopped.getOrElse(LocalDate.now).financialYear
    Map( years.map{ y =>
      y -> loanDetailsProvided.get(y)
    }:_*)
  }
}

object Scheme {

  lazy val nameRegex = """^[a-zA-Z0-9'@,-./() ]*$"""
  lazy val caseRefRegex = """^[a-zA-Z0-9-]*$"""
  lazy val payeRegex = """^\d{3}/[A-Za-z]{2}\d{3}$"""
  lazy val maxNameLength = 50
  lazy val dotaRegex = "[0-9]{8}"
  lazy val yearRegex = "[0-9]{4}"

  val earliestDate: Date = LocalDate.parse("1900-01-01")

  def isInRange(d: LocalDate): Boolean = d.isAfter(earliestDate) && d.isBefore(LocalDate.now())
  def isAfterEarliestDate(d: LocalDate): Boolean = d.isAfter(earliestDate)

  def startBeforeEnd(dates: (LocalDate, LocalDate)): Boolean = (dates._1, dates._2) match {
      //TODO Error throws on same day
    case (start, end) if isInRange(start) && isInRange(end) && start.isBefore(end) => true
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
  ](default: Option[Scheme]): Eff[R, Scheme] = {

    // subjourney for extracting the date range
    def getSchemeDateRange: Eff[R, (Date,Option[Date])] =
      ask[Boolean]("scheme-stillusing").defaultOpt(default.map(_.schemeStopped.isEmpty)) >>= {
        case true => {
          ask[Date]("scheme-stillusingyes")
          .defaultOpt(default.map{_.schemeStart})
              .validating("year-incorrect", {case x => x.getYear.toString.matches(yearRegex)})
          .validating(s"date-far-past", isAfterEarliestDate(_))
          .validating("date-in-future", _.isBefore(LocalDate.now()))
            .in[R] }.map{(_, none[Date])}
        case false => ask[(Date, Date)]("scheme-stillusingno")
          .defaultOpt(default.map{x => (x.schemeStart, x.schemeStopped.get)})
          .validating(
            "year-incorrect",
            {
              case x => x._1.getYear.toString.matches(yearRegex)
              case x => x._2.getYear.toString.matches(yearRegex)
              case _ => true
            }
          )
          .validating("scheme-stillusingno.same-or-after", startBeforeEnd _)
          .in[R].map{ case (k,v) => (k,v.some) }
      }

    // Main journey
    for {
      schemeName            <-  ask[String]("scheme-name")
                                    .defaultOpt(default.map{_.name})
                                  .validating(
                                    "char-limit",
                                    name => name.length < 100
                                  )
                                  .validating(
                                    "invalid-name",
                                    name => name.matches(nameRegex)
                                  )
      //TODO error messages aren't working, need an implicit def like enumeratumHtml
      dotasNumber           <-  ask[YesNoDoNotKnow]("scheme-dotas")
                                  .defaultOpt(default.map{_.dotasReferenceNumber match {
                                    case Some(msg)       => YesNoDoNotKnow.Yes(msg)
                                    case None            => YesNoDoNotKnow.No
                                    case Some("unknown") => YesNoDoNotKnow.DoNotKnow
                                  }})
                                  .validating(
                                    "char-limit",
                                    {
                                      case x: Yes => x.dotas.matches(dotaRegex)
                                      case _ => true
                                    }
                                  )
      schemeReferenceNumber <-  ask[Option[String]]("scheme-refnumber")
                                  .defaultOpt(default.map{_.caseReferenceNumber})
                                  .validating(
                                    "HMRC case reference number must be 10 characters or less",
                                    {
                                      case Some(ref) => ref.length() <= 10
                                      case _ => true
                                    }
                                  )
                                  .validating(
                                    "HMRC case reference number must only include letters a to z, numbers and hyphens",
                                    {
                                      case Some(ref) => ref.matches(caseRefRegex)
                                      case _ => true
                                    }
                                  )

      dateRange             <-  getSchemeDateRange
      employer              <-  ask[Option[Employer]]("scheme-employee")
                                    .validating(
                                      "char-limit",
                                      {
                                        case Some(employer) => employer.name.length <= maxNameLength
                                        case _ => true
                                      }
                                    )
                                    .validating(
                                      "name-format",
                                      {
                                        case Some(employer) => employer.name.matches(nameRegex)
                                        case _ => true
                                      }
                                    )
                                    .validating(
                                      "paye-format",
                                      {
                                        case Some(employer) => employer.paye.matches(payeRegex)
                                        case _ => true
                                      }
                                    )
                                    .in[R]
      recipient             <-  ask[Option[String]]("scheme-recipient")
                                .defaultOpt(default.map{_.loanRecipientName})
                                  .validating(
                                    "char-limit",
                                    {
                                      case Some(name) => name.length <= 50
                                      case _ => true
                                    }
                                  )
                                  .validating(
                                    "name-format",
                                    {
                                      case Some(name) => name.matches(nameRegex)
                                      case _ => true
                                    }
                                  )
                                  .in[R]
      taxNIPaid             <-  ask[Boolean]("scheme-agreedpayment")
                                  .defaultOpt(default.map{_.settlement.isDefined}).in[R]

      settlementStatus      <-  ask[TaxSettlement]("scheme-settlementstatus")
                                  .defaultOpt(default.flatMap{_.settlement})
                                                          .in[R] when taxNIPaid
    } yield {
      import YesNoDoNotKnow._
      val dotas = dotasNumber match {
        case Yes(ref) => Some(ref)
        case No => Some("No")
        case DoNotKnow => Some("Do not know")
      }

      val scheme = Scheme(
        name = schemeName,
        dotasReferenceNumber = dotas,
        caseReferenceNumber = schemeReferenceNumber,
        schemeStart = dateRange._1,
        schemeStopped = dateRange._2,
        employee = employer,
        loanRecipient = recipient.isEmpty,
        loanRecipientName = recipient,
        settlement = settlementStatus
      )
      scheme
    }
  }

}
