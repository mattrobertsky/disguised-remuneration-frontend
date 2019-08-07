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
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.{EmploymentStatus, YesNoDoNotKnow, YesNoUnknown}
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
  settlementAgreed: YesNoUnknown,
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

  def readyToSubmit: Boolean = loanDetails.toList.forall(_._2.isDefined)
}

object Scheme {

  lazy val nameRegex = """^[a-zA-Z0-9',-./() ]*$"""
  lazy val caseRefRegex = """^[a-zA-Z0-9-]*$"""
  lazy val payeRegex = """^\d{3}/[A-Za-z]{2}\d{3}$"""
  lazy val maxNameLength = 50
  lazy val dotaRegex = "[0-9]{8}"
  lazy val yearRegex = "[0-9]{4}"
  lazy val MoneyRegex = """(\d*[.]?\d{1,2})"""

  val earliestDate: Date = LocalDate.parse("1900-01-01")

  def isInRange(d: LocalDate): Boolean = d.isAfter(earliestDate) && d.isBefore(LocalDate.now())
  def isAfterEarliestDate(d: LocalDate): Boolean = d.isAfter(earliestDate)

  def startBeforeEnd(dates: (LocalDate, LocalDate)): Boolean = (dates._1, dates._2) match {
    case (start, end) if isInRange(start) && isInRange(end) && (start.isBefore(end) || start.isEqual(end)) => true
    case _ => false
  }

  type Stack = Fx.fx9[
    UniformAsk[String,?],
    UniformAsk[Option[String],?],
    UniformAsk[Boolean,?],
    UniformAsk[YesNoUnknown,?],
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
    : _uniformAsk[YesNoUnknown,?]
    : _uniformAsk[TaxSettlement,?]
    : _uniformAsk[Boolean,?]
    : _uniformAsk[Option[Employer],?]
    : _uniformAsk[YesNoDoNotKnow,?]
    : _uniformAsk[(Date,Date),?]
    : _uniformAsk[Date,?]
  ](default: Option[Scheme]): Eff[R, Scheme] = {

    // subjourney for extracting the date range
    def getSchemeDateRange(schemeName: String): Eff[R, (Date,Option[Date])] =
      ask[Boolean]("still-using-the-scheme")
        .defaultOpt(default.map(_.schemeStopped.isEmpty))
        .withCustomContentAndArgs(
          ("still-using-the-scheme.heading.hint",
            ("still-using-the-scheme.heading.hint.custom",
              List(schemeName)
            )
          )
        ) >>= {
        case true => {
          ask[Date]("still-using-the-scheme-yes")
          .defaultOpt(default.map{_.schemeStart})
          .withCustomContentAndArgs(
            ("still-using-the-scheme-yes.heading.hint",
              ("still-using-the-scheme-yes.heading.hint.custom",
                List(schemeName)
              )
            )
          )
//          .validating("year-incorrect", x => x.getYear.toString.matches(yearRegex))
          .validating(s"date-far-past", isAfterEarliestDate)
          .validating("date-in-future", _.isBefore(LocalDate.now()))
            .in[R] }.map{(_, none[Date])}
        case false => ask[(Date, Date)]("still-using-the-scheme-no")
          .defaultOpt(default.flatMap{x => x.schemeStopped.map{stop => (x.schemeStart, stop)}})
          .withCustomContentAndArgs(
            ("still-using-the-scheme-no.heading.hint",
              ("still-using-the-scheme-no.heading.hint.custom",
                List(schemeName)
              )
            )
          )
          .validating(
            "year-incorrect",
            {
              case x => x._1.getYear.toString.matches(yearRegex)
              case y => y._2.getYear.toString.matches(yearRegex)
              case _ => true
            }
          )
          .validating("still-using-the-scheme-no.same-or-after", startBeforeEnd)
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
      dotasNumber           <-  ask[YesNoDoNotKnow]("dotas-number")
                                  .defaultOpt(default.map{x => YesNoDoNotKnow(x.dotasReferenceNumber)})
                                  .withCustomContentAndArgs(
                                    ("dotas-number.heading.hint",
                                      ("dotas-number.heading.hint.custom",
                                        List(schemeName)
                                      )
                                    )
                                  )
                                  .validating(
                                    "char-limit",
                                    {
                                      case x: Yes => x.dotas.matches(dotaRegex)
                                      case _ => true
                                    }
                                  )
      schemeReferenceNumber <-  ask[Option[String]]("case-reference-number")
                                  .defaultOpt(default.map{_.caseReferenceNumber})
                                  .withCustomContentAndArgs(
                                    ("case-reference-number.heading.hint",
                                      ("case-reference-number.heading.hint.custom",
                                        List(schemeName)
                                      )
                                    )
                                  )
                                  .validating(
                                    "limit",
                                    {
                                      case Some(ref) => ref.length() <= 10
                                      case _ => true
                                    }
                                  )
                                  .validating(
                                    "format",
                                    {
                                      case Some(ref) => ref.matches(caseRefRegex)
                                      case _ => true
                                    }
                                  )
      dateRange             <-  getSchemeDateRange(schemeName)
      employer              <-  ask[Option[Employer]]("user-employed")
                                  .defaultOpt(default.map{x => x.employee})
                                  .withCustomContentAndArgs(
                                    ("user-employed.heading.hint",
                                      ("user-employed.heading.hint.custom",
                                        List(schemeName)
                                      )
                                    )
                                  )
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
                                  .in[R]
      recipient             <-  ask[Option[String]]("about-loan")
                                  .defaultOpt(default.map{_.loanRecipientName})
                                  .withCustomContentAndArgs(
                                    ("about-loan.heading.hint",
                                      ("about-loan.heading.hint.custom",
                                        List(schemeName)
                                      )
                                    )
                                  )
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
      taxNIPaid             <-  ask[YesNoUnknown]("tax-settled")
                                  .defaultOpt(default.map{_.settlementAgreed})
                                  .withCustomContentAndArgs(
                                    ("tax-settled.heading.hint",
                                      ("tax-settled.heading.hint.custom",
                                        List(schemeName)
                                      )
                                    )
                                  )
                                  .in[R]
      settlementStatus      <-  ask[TaxSettlement]("add-settlement")
                                  .defaultOpt(
                                    default.flatMap{_.settlement}
                                  )
                                  .withCustomContentAndArgs(
                                    ("add-settlement.heading.hint",
                                      ("add-settlement.heading.hint.custom",
                                        List(schemeName)
                                      )
                                    )
                                  )
                                  .validating(
                                    "amount-format",
                                     x => x.amount.matches(MoneyRegex)
                                  )
                                  .in[R] when (taxNIPaid == YesNoUnknown.Yes)
    } yield {

      val scheme = Scheme(
        name = schemeName,
        dotasReferenceNumber = YesNoDoNotKnow.unapply(dotasNumber),
        caseReferenceNumber = schemeReferenceNumber,
        schemeStart = dateRange._1,
        schemeStopped = dateRange._2,
        employee = employer,
        loanRecipient = recipient.isEmpty,
        loanRecipientName = recipient,
        settlementAgreed = taxNIPaid,
        settlement = settlementStatus
      )
      scheme
    }
  }

}
