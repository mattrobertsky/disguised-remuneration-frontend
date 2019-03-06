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
      stillUsingYes         <-  ask[Date]("scheme-stillusingyes").in[R] when stillUsingScheme
      stillUsingNo          <-  ask[(Date, Date)]("scheme-stillusingno").in[R] when !stillUsingScheme
      employee              <-  ask[Boolean]("scheme-employee").in[R] when !stillUsingScheme
      recipient             <-  ask[Boolean]("scheme-recipient").in[R] when !stillUsingScheme
      taxNIPaid             <-  ask[Boolean]("scheme-agreedpayment").in[R] when !stillUsingScheme
      settlementStatus      <-  ask[String]("scheme-settlementstatus").in[R] when taxNIPaid == Some(true)
    } yield {

      println(s"$schemeName")
      println(s"$dotasNumber")
      println(s"$schemeReferenceNumber")
      println(s"$stillUsingScheme")
      if (stillUsingScheme)
        println(s"$stillUsingYes")
      else
        println(s"$stillUsingNo")
      println(s"$employee")
      println(s"$recipient")
      println(s"$taxNIPaid")
      println(s"$settlementStatus")

      val scheme = Scheme(
        name = schemeName,
        dotasReferenceNumber = Some("dotas1"),
        caseReferenceNumber = Some("case Ref1"),
        schemeStart = Some(java.time.LocalDate.now()),
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
