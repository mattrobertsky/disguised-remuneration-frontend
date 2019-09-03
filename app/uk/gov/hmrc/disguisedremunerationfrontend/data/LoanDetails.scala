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
import play.api.i18n.Messages
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.{YesNoUnknown, YesNoUnknownWrittenOff}
import uk.gov.hmrc.disguisedremunerationfrontend.data.Scheme.moneyRegex

import scala.language.higherKinds
import cats.data.NonEmptyList
import cats.implicits._
import ltbs.uniform.{::, Language, NilTypes, _}
import play.api.i18n.{Messages => _}

case class LoanDetails(
    year: Int,
    hmrcApproved: Option[YesNoUnknown],
    totalLoan: TotalLoan,
    isGenuinelyRepaid: Boolean,
    genuinelyRepaid: Option[Money],
    isWrittenOff: Option[List[String]],
    writtenOff: Option[WrittenOff]
)

object LoanDetails {
  type TellTypes = NilTypes
  type AskTypes = YesNoUnknown :: Boolean :: Money :: WrittenOff :: YesNoUnknownWrittenOff :: TotalLoan :: NilTypes

  def loanDetailsProgram[F[_]: cats.Monad](
    interpreter: Language[F, TellTypes, AskTypes],
    year: Int,
    messages: Messages,
    schemeName: String,
    default: Option[LoanDetails] = None
  ): F[LoanDetails] = {
    import interpreter._
    implicit val implMess = messages
    val (startDate, endDate) = year.toFinancialYear
    for {
      approved <- ask[YesNoUnknown](
        "fixed-term-loan",
        default = default.flatMap(x => x.hmrcApproved),
        customContent = Map(
          "fixed-term-loan.heading.hint" ->
            Tuple2("fixed-term-loan.heading.hint", List(schemeName)))
      ) when startDate.isBefore(LocalDate.of(2010, 4, 6))

      amount <- ask[TotalLoan](
        "loan-amount",
        default = default.map(_.totalLoan),
        customContent = Map(
          "loan-amount.amount.required" ->
            Tuple2("loan-amount.amount.required", List(formatDate(startDate), formatDate(endDate))),
          "loan-amount.heading" ->
            Tuple2("loan-amount.heading", List(formatDate(startDate), formatDate(endDate))),
          "loan-amount.heading.hint" ->
            Tuple2("loan-amount.heading.hint", List(schemeName))
        ),
        validation = List(
          List(
            Rule.fromPred(
              totalLoan => totalLoan.amount.matches(moneyRegex),
              (ErrorMsg("format"), NonEmptyList.one(List("amount")))
            ),
            Rule.fromPred(
              totalLoan => totalLoan.amount.nonEmpty,
              (ErrorMsg("required"), NonEmptyList.one(List("amount")))
            ),
            Rule.fromPred(
              totalLoan => totalLoan.estimate || !totalLoan.estimate,
              (ErrorMsg("required"), NonEmptyList.one(List("estimate")))
            )
          )
        )
      )

      isRepaid <- ask[Boolean](
        "repaid-any-loan-during-tax-year",
        default = default.map(_.isGenuinelyRepaid),
        customContent = Map(
          "repaid-any-loan-during-tax-year.heading" ->
            Tuple2("repaid-any-loan-during-tax-year.heading", List(formatDate(startDate), formatDate(endDate))),
          "repaid-any-loan-during-tax-year.required" ->
            Tuple2("repaid-any-loan-during-tax-year.required", List(formatDate(startDate), formatDate(endDate))),
          "repaid-any-loan-during-tax-year.heading.hint" ->
            Tuple2("repaid-any-loan-during-tax-year.heading.hint", List(schemeName))
          )
      )

      repaid <- ask[Money](
        "loan-repaid",
        default = default.map(_.genuinelyRepaid.getOrElse("")),
        customContent = Map(
          "loan-repaid.heading.hint" ->
          Tuple2("loan-repaid.heading.hint", List(schemeName))),
        validation = List(
          List(
            Rule.fromPred(
              repaid => repaid.matches(moneyRegex),
              (ErrorMsg("format"), NonEmptyList.one(Nil))
            ),
            Rule.fromPred(
              repaid => repaid.nonEmpty,
              (ErrorMsg("required"), NonEmptyList.one(Nil))
            )
          )
        )
      ) when isRepaid

      isWrittenOff <- ask[YesNoUnknownWrittenOff](
        "written-off",
        default = default.map(x => x.isWrittenOff match {
          case Some(x: List[String]) if x == List(YesNoUnknown.CUnknown.entryName) => YesNoUnknownWrittenOff.Unknown
          case Some(x: List[String]) if x == List(YesNoUnknown.BNo.entryName) => YesNoUnknownWrittenOff.No
          case Some(x: List[String]) => YesNoUnknownWrittenOff(x.some)
        }),
        customContent = Map(
          "written-off.heading" ->
            Tuple2("written-off.heading", List(formatDate(startDate), formatDate(endDate))),
          "written-off.required" ->
            Tuple2("written-off.required", List(formatDate(startDate), formatDate(endDate))),
          "written-off.heading.hint" ->
            Tuple2("written-off.heading.hint", List(schemeName))
        ),
        validation = List(
          List(
            Rule.fromPred(
              {
                case YesNoUnknownWrittenOff.Yes(writtenOff) => writtenOff.amount.matches(moneyRegex)
                case _ => true
              },
              (ErrorMsg("format"), NonEmptyList.one(List("Yes", "writtenOff", "amount")))
            ),
            Rule.fromPred(
              {
                case YesNoUnknownWrittenOff.Yes(writtenOff) => writtenOff.amount.nonEmpty
                case _ => true
              },
              (ErrorMsg("required"), NonEmptyList.one(List("Yes", "writtenOff", "amount")))
            ),
            Rule.fromPred(
              {
                case YesNoUnknownWrittenOff.Yes(writtenOff) => writtenOff.taxPaid.matches(moneyRegex)
                case _ => true
              },
              (ErrorMsg("format"), NonEmptyList.one(List("Yes", "writtenOff", "taxPaid")))
            ),
            Rule.fromPred(
              {
                case YesNoUnknownWrittenOff.Yes(writtenOff) => writtenOff.taxPaid.nonEmpty
                case _ => true
              },
              (ErrorMsg("required"), NonEmptyList.one(List("Yes", "writtenOff", "taxPaid")))
            )
          )
        )
      )
      writtenOff = isWrittenOff match {
        case YesNoUnknownWrittenOff.Yes(isWrittenOff) => Some(WrittenOff(isWrittenOff.amount, isWrittenOff.taxPaid))
        case _ => None
      }
    } yield {
      LoanDetails(year,
                  approved,
                  amount,
                  isRepaid,
                  repaid,
                  YesNoUnknownWrittenOff.unapply(isWrittenOff),
                  writtenOff
      )
    }
  }
}
