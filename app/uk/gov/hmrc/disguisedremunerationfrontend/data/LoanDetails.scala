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

case class LoanDetails(
  year: Int,
  hmrcApproved: Boolean,
  amount: Money,
  genuinelyRepaid: Money,
  writtenOff: Option[WrittenOff]
)

import cats.implicits._
import org.atnos.eff._
import ltbs.uniform._

object LoanDetails {

  type Stack = Fx3[
    UniformAsk[Boolean,?],
    UniformAsk[Money,?],
    UniformAsk[WrittenOff,?]
  ]

  def program[R
    : _uniformCore
    : _uniformAsk[Boolean,?]
    : _uniformAsk[Money,?]
    : _uniformAsk[WrittenOff,?]
  ](year: Int, default: Option[LoanDetails] = None): Eff[R, LoanDetails] = {
    val (startDate, endDate) = year.toFinancialYear
    (
      ask[Boolean]("details-hmrc-approved")
        .defaultOpt(default.map(_.hmrcApproved)).in[R],
      ask[Money]("details-amount")
        .defaultOpt(default.map(_.amount))
        .withCustomContentAndArgs(
          // replace details-amount.heading with
          // details-amount.heading.range and apply the arguments
          ("details-amount.heading",("details-amount.heading.range", List(startDate, endDate)))
        ).in[R],
      ask[Money]("details-genuinely-repaid-amount")
        .defaultOpt(default.map(_.genuinelyRepaid)).in[R] emptyUnless
        ask[Boolean]("details-genuinely-repaid")
        .defaultOpt(default.map(_.genuinelyRepaid != 0)).in[R],

      ask[WrittenOff]("details-written-off-amount")
        .defaultOpt(default.flatMap(_.writtenOff)).in[R] when
        ask[Boolean]("details-written-off")
        .defaultOpt(default.map(_.writtenOff.isDefined)).in[R]

    ).mapN(LoanDetails.apply)
  }
}
