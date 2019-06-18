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
import org.atnos.eff._
import play.api.i18n.Messages
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.YesNoUnknown
import uk.gov.hmrc.disguisedremunerationfrontend.data.Scheme.MoneyRegex

case class LoanDetails(
  year: Int,
  hmrcApproved: YesNoUnknown,
  amount: Money,
  genuinelyRepaid: Option[Money],
  isWrittenOff: YesNoUnknown,
  writtenOff: Option[WrittenOff]
) {
  // TODO move this to a pimp
  def toListString = {
    List(
      "£" ++ amount.toString,
      genuinelyRepaid.fold("£" ++ "0")(gr => "£" ++ gr.toString),
      writtenOff.fold("£" ++ "0")(wo =>"£" ++ wo.amount.toString),
      writtenOff.fold("£" ++ "0")(wo =>"£" ++ wo.taxPaid.toString)
    )
  }


}

object LoanDetails {

  type Stack = Fx.fx5[
    UniformAsk[YesNoUnknown, ?],
    UniformAsk[Boolean, ?],
    UniformAsk[Money, ?],
    UniformAsk[Option[Money], ?],
    UniformAsk[WrittenOff, ?]
    ]

  def program[R
  : _uniformCore
  : _uniformAsk[YesNoUnknown, ?]
  : _uniformAsk[Boolean, ?]
  : _uniformAsk[Money, ?]
  : _uniformAsk[Option[Money], ?]
  : _uniformAsk[WrittenOff, ?]
  ](year: Int, scheme: Scheme, default: Option[LoanDetails] = None)(implicit messages: Messages) = {
    val (startDate, endDate) = year.toFinancialYear
    for {
      approved <- ask[YesNoUnknown]("details-hmrc-approved")
        .defaultOpt(default.map(_.hmrcApproved))
        .withCustomContentAndArgs(
            ("details-hmrc-approved.heading.hint",
              ("details-hmrc-approved.heading.hint.custom",
                List(scheme.name)
              )
            )
          ).in[R]
      amount <- ask[Money]("details-amount")
        .defaultOpt(default.map(_.amount))
        .validating(
          "format",
          x => x.matches(MoneyRegex)
        )
        .withCustomContentAndArgs(
          ("details-amount.required",
            ("details-amount.required",
            List(formatDate(startDate),
            formatDate(endDate))
        ))
        )
        .withCustomContentAndArgs(
          ("details-amount.heading",
            ("details-amount.heading.range",
              List(formatDate(startDate),
                formatDate(endDate))
            ))
        )
        .withCustomContentAndArgs(
          ("details-amount.heading.hint",
            ("details-amount.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R]
      repaid <-  ask[Money]("details-genuinely-repaid-amount")
        .defaultOpt(default.flatMap(_.genuinelyRepaid))
        .validating(
          "format",
          x => x.matches(MoneyRegex)
        )
        .withCustomContentAndArgs(
          ("details-genuinely-repaid-amount.heading.hint",
            ("details-genuinely-repaid-amount.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R] when
        ask[Boolean]("details-genuinely-repaid")
          .defaultOpt(default.map(_.genuinelyRepaid != 0))
          .withCustomContentAndArgs(
            ("details-genuinely-repaid.heading",
              ("details-genuinely-repaid.heading.range",
                List(formatDate(startDate),
                  formatDate(endDate))
              ))
          )
          .withCustomContentAndArgs(
            ("details-genuinely-repaid.required",
              ("details-genuinely-repaid.required",
                List(formatDate(startDate),
                  formatDate(endDate))
              ))
          )
          .withCustomContentAndArgs(
            ("details-genuinely-repaid.heading.hint",
              ("details-genuinely-repaid.heading.hint.custom",
                List(scheme.name)
              )
            )
          ).in[R]
      isWrittenOff <- ask[YesNoUnknown]("details-written-off")
        .defaultOpt(default.map(_.isWrittenOff))
        .withCustomContentAndArgs(
          ("details-written-off.heading",
            ("details-written-off.heading.range",
              List(formatDate(startDate),
                formatDate(endDate))
            ))
        )
        .withCustomContentAndArgs(
          ("details-written-off.required",
            ("details-written-off.required",
              List(formatDate(startDate),
                formatDate(endDate))
            ))
        )
        .withCustomContentAndArgs(
          ("details-written-off.heading.hint",
            ("details-written-off.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R]
      writtenOff <- ask[WrittenOff]("details-written-off-amount")
        .defaultOpt(default.flatMap(_.writtenOff))
        //TODO Need a feature to validate both together, right now it is sequential
//          .validating(
//            "both-format",
//            {
//              case WrittenOff(amount, tax) => amount.matches(MoneyRegex) && tax.matches(MoneyRegex)
//              case _ => true
//            }
//          )
        .validating(
          "amount-format",
            x => x.amount.matches(MoneyRegex)
        )
        .validating(
          "tax-format",
           x => x.taxPaid.matches(MoneyRegex)
        )
        .withCustomContentAndArgs(
          ("details-written-off-amount.heading.hint",
            ("details-written-off-amount.heading.hint.custom",
              List(scheme.name)
            )
          )
        ).in[R] when (isWrittenOff == YesNoUnknown.Yes)

    } yield {
      LoanDetails(year, approved, amount, repaid, isWrittenOff, writtenOff)
    }
  }
}

