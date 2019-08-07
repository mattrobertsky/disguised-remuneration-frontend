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
import org.atnos.eff._
import play.api.i18n.Messages
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoUnknown
import uk.gov.hmrc.disguisedremunerationfrontend.data.Scheme.MoneyRegex

case class LoanDetails(
  year: Int,
  hmrcApproved: Option[YesNoUnknown],
  totalLoan: TotalLoan,
  isGenuinelyRepaid: Boolean,
  genuinelyRepaid: Option[Money],
  isWrittenOff: YesNoUnknown,
  writtenOff: Option[WrittenOff]
) {

  def toListString: List[String] = {
    List(
      "£" ++ totalLoan.amount.toString,
      if(totalLoan.estimate)
        "Yes"
      else
        "No"
      ,
      genuinelyRepaid.fold("£" ++ "0")(gr => "£" ++ gr.toString),
      writtenOff.fold("£" ++ "0")(wo =>"£" ++ wo.amount.toString),
      writtenOff.fold("£" ++ "0")(wo =>"£" ++ wo.taxPaid.toString)
    )
  }
}


object LoanDetails {

  type Stack = Fx.fx6[
    UniformAsk[YesNoUnknown, ?],
    UniformAsk[TotalLoan, ?],
    UniformAsk[Boolean, ?],
    UniformAsk[Money, ?],
    UniformAsk[Option[Money], ?],
    UniformAsk[WrittenOff, ?]
    ]

  def program[R
  : _uniformCore
  : _uniformAsk[YesNoUnknown, ?]
  : _uniformAsk[TotalLoan, ?]
  : _uniformAsk[Boolean, ?]
  : _uniformAsk[Money, ?]
  : _uniformAsk[Option[Money], ?]
  : _uniformAsk[WrittenOff, ?]
  ](year: Int, scheme: Scheme, default: Option[LoanDetails] = None)(implicit messages: Messages): Eff[R, LoanDetails] = {
    val (startDate, endDate) = year.toFinancialYear
    for {
      approved <- ask[YesNoUnknown]("fixed-term-loan")
        .defaultOpt(default.flatMap(_.hmrcApproved))
        .withCustomContentAndArgs(
            ("fixed-term-loan.heading.hint",
              ("fixed-term-loan.heading.hint.custom",
                List(scheme.name)
              )
            )
          ).in[R] when startDate.isBefore(LocalDate.of(2010, 4, 6))
      amount <- ask[TotalLoan]("loan-amount")
        .defaultOpt(default.map(_.totalLoan)
        )
        .validating(
          "format",
          x => x.amount.matches(MoneyRegex)
        )
        .withCustomContentAndArgs(
          ("loan-amount.amount.required",
            ("loan-amount.amount.required",
            List(formatDate(startDate),
            formatDate(endDate))
            )
          )
        )
        .withCustomContentAndArgs(
          ("loan-amount.heading",
            ("loan-amount.heading",
              List(formatDate(startDate),
                formatDate(endDate))
            )
          )
        )
        .withCustomContentAndArgs(
          ("loan-amount.heading.hint",
            ("loan-amount.heading.hint",
              List(scheme.name)
            )
          )
        ).in[R]
      isRepaid <- ask[Boolean]("repaid-any-loan-during-tax-year")
        .defaultOpt(default.map(_.isGenuinelyRepaid))
        .withCustomContentAndArgs(
          ("repaid-any-loan-during-tax-year.heading",
            ("repaid-any-loan-during-tax-year.heading.range",
              List(formatDate(startDate),
                formatDate(endDate))
            ))
        )
        .withCustomContentAndArgs(
          ("repaid-any-loan-during-tax-year.required",
            ("repaid-any-loan-during-tax-year.required",
              List(formatDate(startDate),
                formatDate(endDate))
            ))
        )
        .withCustomContentAndArgs(
          ("repaid-any-loan-during-tax-year.heading.hint",
            ("repaid-any-loan-during-tax-year.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R]
      repaid <-  ask[Money]("loan-repaid")
        .defaultOpt(default.flatMap(_.genuinelyRepaid))
        .validating(
          "format",
          x => x.matches(MoneyRegex)
        )
        .withCustomContentAndArgs(
          ("loan-repaid.heading.hint",
            ("loan-repaid.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R] when isRepaid
      isWrittenOff <- ask[YesNoUnknown]("written-off")
        .defaultOpt(default.map(_.isWrittenOff))
        .withCustomContentAndArgs(
          ("written-off.heading",
            ("written-off.heading.range",
              List(formatDate(startDate),
                formatDate(endDate))
            ))
        )
        .withCustomContentAndArgs(
          ("written-off.required",
            ("written-off.required",
              List(formatDate(startDate),
                formatDate(endDate))
            ))
        )
        .withCustomContentAndArgs(
          ("written-off.heading.hint",
            ("written-off.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R]
      writtenOff <- ask[WrittenOff]("written-off-amount")
        .defaultOpt(default.flatMap(_.writtenOff))
        .validating(
          "amount-format",
            x => x.amount.matches(MoneyRegex)
        )
        .validating(
          "tax-format",
           x => x.taxPaid.matches(MoneyRegex)
        )
        .withCustomContentAndArgs(
          ("written-off-amount.heading.hint",
            ("written-off-amount.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R] when (isWrittenOff == YesNoUnknown.Yes)

    } yield {
      LoanDetails(year, approved, amount, isRepaid, repaid, isWrittenOff, writtenOff)
    }
  }
}

