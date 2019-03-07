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
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration.Date


case class Scheme(
  name: String,
  dotasReferenceNumber: Option[String],
  caseReferenceNumber: Option[String],
  schemeStart: Option[Date],
  schemeStopped: Option[Date],
  employee: Boolean,
  loanRecipient: Boolean,
  loanRecipientName: Option[String],
  settlement: TaxSettlement
)


object Scheme {

  val earliestDate = LocalDate.parse("1999-04-05")
//  def isBeforeEarliest(date: LocalDate) =
  def isInRange(d: LocalDate) = d.isAfter(earliestDate) && d.isBefore(LocalDate.now())

  def startBeforeEnd(dates: (LocalDate, LocalDate)): Boolean = (dates._1, dates._2) match {
    case (start, end) if (isInRange(start) && isInRange(end) && start.isBefore(end)) => true
    case _ => false
  }


  type Stack = Fx.fx5[
    UniformAsk[String,?],
    UniformAsk[Boolean,?],
    UniformAsk[YesNoDoNotKnow,?],
    UniformAsk[Date,?],
    UniformAsk[(Date, Date),?]
    ]

  def program[R
    : _uniformCore
    : _uniformAsk[String,?]
    : _uniformAsk[Boolean,?]
    : _uniformAsk[YesNoDoNotKnow,?]
    : _uniformAsk[(Date,Date),?]
    : _uniformAsk[Date,?]
  ]: Eff[R, Option[Scheme]] =
    for {
      schemeName            <-  ask[String]("scheme-name")
      dotasNumber           <-  ask[YesNoDoNotKnow]("scheme-dotas")
      schemeReferenceNumber <-  ask[Boolean]("scheme-refnumber")
      stillUsingScheme      <-  ask[Boolean]("scheme-stillusing")
      stillUsingYes         <-  ask[Date]("scheme-stillusingyes")
                                  .validating(s"The date you started using the scheme must after $earliestDate", isInRange(_))
                                  .validating("The date you started using the scheme must be in the past", _.isBefore(LocalDate.now()))
                                  .in[R] when stillUsingScheme
      stillUsingNo          <-  ask[(Date, Date)]("scheme-stillusingno")
                                  .validating("The date you stopped using the scheme must be the same as or after the date you started using the scheme", startBeforeEnd _)
                                  .in[R] when !stillUsingScheme
      employee              <-  ask[Boolean]("scheme-employee").in[R] when !stillUsingScheme
      recipient             <-  ask[Boolean]("scheme-recipient").in[R] when !stillUsingScheme
      taxNIPaid             <-  ask[Boolean]("scheme-agreedpayment").in[R] when !stillUsingScheme
      settlementStatus      <-  ask[String]("scheme-settlementstatus").in[R] when taxNIPaid == Some(true)
    } yield {

      val scheme = Scheme(
        name = schemeName,
        dotasReferenceNumber = Some("dotas1"),
        caseReferenceNumber = Some("case Ref1"),
        schemeStart = Some(LocalDate.now()),
        schemeStopped = None,
        employee = true,
        loanRecipient = true,
        loanRecipientName = Some("Shadey accountant"),
        settlement = new TaxSettlement(123)
      )
      println(s"scheme: $scheme")
      Some(scheme)
    }

}
