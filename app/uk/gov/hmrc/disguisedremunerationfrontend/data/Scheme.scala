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
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration.Date

case class Scheme(
  name: String //,
//  referenceNumber: Option[String],
//  caseReferenceNumber: Option[String],
//  schemeReferenceNumber: Option[String],
//  schemeStart: Date,
//  stoppedInvolvmentOn: Option[Date],
//  bankrupted: Boolean,
//  employee: Boolean,
//  loanRecipient: Boolean,
//  taxSettlements: List[TaxSettlement]
)


object Scheme {

  type Stack = Fx.fx1[
    UniformAsk[String,?]
//    ,
//    UniformAsk[Option[String],?],
//    UniformAsk[Date,?],
//    UniformAsk[Option[Date],?],
//    UniformAsk[Boolean,?],
//    UniformAsk[Option[TaxSettlement],?]
    ]

  def program[R
    : _uniformCore
  : _uniformAsk[String,?]
//  : _uniform[Option[String],?]
//  : _uniform[Date,?]
//  : _uniform[Option[Date],?]
//  : _uniform[Boolean,?]
//  : _uniform[Option[TaxSettlement],?]
  ]: Eff[R, Scheme] = for {
    name                  <- ask[String]("scheme-namex")
//    schemeReferenceNumber <- uask[R,Option[String]]("schemeReferenceNumber")
//    caseReferenceNumber   <- uask[R,Option[String]]("caseReferenceNumber")
//    otherReferenceNumber  <- uask[R,Option[String]]("otherReferenceNumber")
//    schemeStart           <- uask[R,Date]("schemeStart")
//    stoppedInvolvmentOn   <- uask[R,Option[Date]]("stoppedInvolvmentOn")
//    bankrupted            <- uask[R,Boolean]("bankrupted")
//    employee              <- uask[R,Boolean]("employee")
//    loanRecipient         <- uask[R,Boolean]("loanRecipient")
//    taxSettlements        <- uask[R,Option[TaxSettlement]]("taxSettlements").map{_.toList}
  } yield Scheme(
    name
//    schemeReferenceNumber,
//    caseReferenceNumber,
//    otherReferenceNumber,
//    schemeStart,
//    stoppedInvolvmentOn,
//    bankrupted,
//    employee,
//    loanRecipient,
//    taxSettlements
  )

}
