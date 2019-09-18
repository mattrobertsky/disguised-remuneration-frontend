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

import cats.data.NonEmptyList
import cats.implicits._
import ltbs.uniform.{::, Language, NilTypes, _}
import play.api.i18n.{Messages => _}
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.{YesNoDoNotKnow, YesNoUnknown, YesNoUnknownWrittenOff}

import scala.language.higherKinds

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
  lazy val dotaRegex = "[0-9]{8}|.{0}"
  lazy val yearRegex = "[0-9]{4}"
//  No longer needed, but handy for future reference.
//  lazy val moneyRegex = """(\d*[.]?\d{1,2}|.{0})"""

  type TellTypes = NilTypes
  type AskTypes = String :: Option[String] :: Date :: (Date, Date) :: Option[Employer] :: Boolean :: TaxSettlement :: YesNoDoNotKnow :: YesNoUnknown :: NilTypes


// TODO Check business logic on the earliest date
//
//  val earliestDate: Date = LocalDate.parse("1900-01-01")
//  def isInRange(d: LocalDate): Boolean = d.isAfter(earliestDate) && d.isBefore(LocalDate.now())
//  def isAfterEarliestDate(d: LocalDate): Boolean = d.isAfter(earliestDate)

  def startBeforeEnd(dates: (LocalDate, LocalDate)): Boolean = (dates._1, dates._2) match {
    case (start, end) if start.isBefore(end) || start.isEqual(end) => true
    case _ => false
  }

  private def headingHint(key: String, schemeName: String) = Map(key ->
    Tuple2(key,
      List(schemeName)
    ))

  def schemeProgram[F[_] : cats.Monad](
    interpreter: Language[F, TellTypes, AskTypes],
    default: Option[Scheme]
  ): F[Scheme] = {
    import interpreter._
    for {
      schemeName            <-  ask[String](
                                  "scheme-name",
                                  default = default.map(_.name),
                                  validation = List(List(
                                    Rule.fromPred(
                                      _.matches(nonEmptyStringRegex),
                                      (ErrorMsg("required"), NonEmptyList.one(Nil))
                                    ),
                                    Rule.fromPred[String](
                                      _.length < 100,
                                      (ErrorMsg("char-limit"), NonEmptyList.one(Nil))
                                    ),
                                    Rule.fromPred[String](
                                      _.matches(nameRegex),
                                      (ErrorMsg("invalid-name"), NonEmptyList.one(Nil))
                                    )
                                )))
      dotasNumber           <-  ask[YesNoDoNotKnow](
                                  id = "dotas-number",
                                  default = default.map(x => x.dotasReferenceNumber match {
                                    case Some(x: String) if x == YesNoDoNotKnow.DoNotKnow.toString => YesNoDoNotKnow.DoNotKnow
                                    case Some(x: String) if x == YesNoDoNotKnow.No.toString => YesNoDoNotKnow.No
                                    case Some(x: String) => YesNoDoNotKnow(x.some)
                                  }),
                                  customContent = headingHint("dotas-number.heading.hint", schemeName),
                                  validation = List(List(
                                    Rule.fromPred(
                                      {
                                        case x:YesNoDoNotKnow.Yes => x.dotas.matches(nonEmptyStringRegex)
                                        case _ => true
                                      },
                                      (ErrorMsg("required"), NonEmptyList.one(List("Yes", "dotas")))
                                    ),
                                    Rule.fromPred({
                                      case x:YesNoDoNotKnow.Yes => x.dotas.matches(dotaRegex)
                                      case _ => true
                                    },
                                    (ErrorMsg("char-limit"), NonEmptyList.one(List("Yes", "dotas"))))
                                  ))
                                )
      schemeReferenceNumber <-  ask[Option[CaseRef]](
                                  id = "case-reference-number",
                                  default = default.map(_.caseReferenceNumber),
                                  customContent = headingHint("case-reference-number.heading.hint", schemeName),
                                  validation = List(
                                    List(
                                      Rule.fromPred(
                                        {
                                          case Some(ref) => ref.matches(nonEmptyStringRegex)
                                          case _ => true
                                        },
                                        (ErrorMsg("required"), NonEmptyList.one(List("Some", "value")))
                                      ),
                                      Rule.fromPred(
                                        {
                                          case Some(ref) => ref.length <= 12
                                          case _ => true
                                        },
                                        (ErrorMsg("limit"), NonEmptyList.one(List("Some", "value")))
                                      )
                                  ),
                                    List(
                                      Rule.fromPred(
                                        {
                                          case Some(ref) => ref.matches(caseRefRegex)
                                          case _ => true
                                        },
                                        (ErrorMsg("format"), NonEmptyList.one(List("Some", "value")))
                                      )
                                    )
                                  )
                                )
      stillUsingScheme      <-  ask[Boolean](
                                  "still-using-the-scheme",
                                  default.map(x => x.schemeStopped.isEmpty),
                                  customContent = headingHint("still-using-the-scheme.heading.hint", schemeName)
                                )
      dateRange             <-  if (stillUsingScheme) {
                                  ask[Date](
                                    "still-using-the-scheme-yes",
                                    default.map(_.schemeStart),
                                    validation = List(List(
                                      Rule.fromPred(
                                        {
                                          date: Date => date.getYear.toString.length == 4
                                        },
                                        (ErrorMsg("not-a-date"), NonEmptyList.one(Nil))),
                                      //TODO Check business rules on this
//                                      Rule.fromPred(
//                                        {
//                                          date: Date => isAfterEarliestDate(date)
//                                        },
//                                        (ErrorMsg("date-far-past"), NonEmptyList.one(Nil))),
                                      Rule.fromPred(
                                        {
                                          date: Date => date.isBefore(LocalDate.now)
                                        },
                                        (ErrorMsg("date-in-future"), NonEmptyList.one(Nil)))
                                      )
                                    ),
                                    customContent = headingHint("still-using-the-scheme-yes.heading.hint", schemeName)
                                  ).map(x => (x, none[Date]))
                                } else {
                                  ask[(Date, Date)](
                                    "still-using-the-scheme-no",
                                    default.flatMap{x => x.schemeStopped.map{stop => (x.schemeStart, stop)}},
                                    validation = List(List(
                                      Rule.fromPred[(Date,Date)](
                                        {
                                          d:(Date,Date) => d._1.getYear.toString.length == 4
                                        },
                                        (ErrorMsg("not-a-date"), NonEmptyList.one(List("_1")))
                                      ),
                                      Rule.fromPred[(Date,Date)](
                                        {
                                          d:(Date,Date) => d._2.getYear.toString.length == 4
                                        },
                                        (ErrorMsg("not-a-date"), NonEmptyList.one(List("_2")))
                                      ),
                                      Rule.fromPred[(Date,Date)](
                                        {
                                          d:(Date,Date) => startBeforeEnd(d)
                                        },
                                        (ErrorMsg("same-or-after"), NonEmptyList.one(List("_1")))
                                      ),
                                      Rule.fromPred[(Date,Date)](
                                        {
                                          d:(Date,Date) => startBeforeEnd(d)
                                        },
                                        (ErrorMsg("same-or-after"), NonEmptyList.one(List("_2")))
                                      ),
                                      Rule.fromPred(
                                        {
                                          date: (Date, Date) => date._1.isBefore(LocalDate.now)
                                        },
                                        (ErrorMsg("date-in-future"), NonEmptyList.one(List("_1")))),
                                      Rule.fromPred(
                                        {
                                          date: (Date, Date) => date._2.isBefore(LocalDate.now)
                                        },
                                        (ErrorMsg("date-in-future"), NonEmptyList.one(List("_2"))))
                                    )),
                                    customContent = headingHint("still-using-the-scheme-no.heading.hint", schemeName)
                                  ).map (x => (x._1, x._2.some))
                                }
      employer              <-  ask[Option[Employer]](
                                  "user-employed",
                                  default.map(_.employee),
                                  customContent = headingHint("user-employed.heading.hint", schemeName),
                                  validation = List(
                                    List(
                                      Rule.fromPred(
                                        {
                                          case Some(employer) => employer.name.matches(nonEmptyStringRegex)
                                          case _ => true
                                        },
                                        (ErrorMsg("required"), NonEmptyList.one(List("Some", "value", "name")))
                                      ),
                                          Rule.fromPred(
                                            {
                                              case Some(employer) => employer.name.length <= maxNameLength
                                              case _ => true
                                            },
                                            (ErrorMsg("char-limit"), NonEmptyList.one(List("Some", "value", "name")))
                                          )
                                    ),
                                    List(
                                      Rule.fromPred(
                                        {
                                          case Some(employer) => employer.name.matches(nameRegex)
                                          case _ => true
                                        },
                                        (ErrorMsg("format"), NonEmptyList.one(List("Some", "value", "name")))
                                      )
                                    )
                                  )
                                )
      recipient             <-  ask[Option[String]](
                                  "about-loan", default.map{_.loanRecipientName},
                                  customContent = headingHint("about-loan.heading.hint", schemeName),
                                  validation = List(
                                    List(
                                      Rule.fromPred(
                                        {
                                          case Some(name) => name.matches(nonEmptyStringRegex)
                                          case _ => true
                                        },
                                        (ErrorMsg("required"), NonEmptyList.one(List("Some", "value")))
                                      ),
                                      Rule.fromPred(
                                        {
                                          case Some(name) => name.length <= 50
                                          case _ => true
                                        },
                                        (ErrorMsg("name-length"), NonEmptyList.one(List("Some", "value")))
                                      )
                                    ),
                                    List(
                                      Rule.fromPred(
                                        {
                                          case Some(name) => name.matches(nameRegex)
                                          case _ => true
                                        },
                                        (ErrorMsg("name-format"), NonEmptyList.one(List("Some", "value")))
                                      )
                                    )
                                  )
                                )
      taxNIPaid             <-  ask[YesNoUnknown](
                                  "tax-settled",
                                  default.map{_.settlementAgreed},
                                  customContent = headingHint("tax-settled.heading.hint", schemeName)
                                )
      settlementStatus      <-  ask[TaxSettlement](
                                  "add-settlement",
                                  default.flatMap{_.settlement},
                                  customContent = headingHint("add-settlement.heading.hint", schemeName)
                                ) when taxNIPaid == YesNoUnknown.AYes
    } yield {
      Scheme(
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
    }
  }
}
