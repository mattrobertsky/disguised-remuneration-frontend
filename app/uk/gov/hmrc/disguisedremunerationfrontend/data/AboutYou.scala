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
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.EmploymentStatus
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration._

case class AboutYou(
  completedBySelf: Boolean,
  alive: Boolean,
  identification: Option[Either[Nino, Utr]] = None,
  deceasedBefore: Option[Boolean] = None,
  employmentStatus: Option[EmploymentStatus] = None,
  actingFor: Option[String] = None
)

object AboutYou {

  sealed trait Error
  case object NoNeedToComplete extends Error

  type Stack = Fx.fx5[
    UniformAsk[Boolean,?],
    UniformAsk[String,?],
    UniformAsk[Unit,?],
    UniformAsk[Either[Nino,Utr],?],
    UniformAsk[EmploymentStatus,?]
  ]

  def program[R
      : _uniformCore
      : _uniformAsk[Boolean,?]
      : _uniformAsk[String,?]
      : _uniformAsk[Either[Nino,Utr],?]
      : _uniformAsk[EmploymentStatus,?]
      : _uniformAsk[Unit,?]
  ]: Eff[R, Either[Error,Option[AboutYou]]] = 
    for {
      completedBy <- ask[Boolean]("aboutyou-completedby")
      ret <- completedBy match {
        case false => Eff.pure[R,Either[Error,Option[AboutYou]]](Right(None))
        case true => for {
          alive   <- ask[Boolean]("aboutyou-personalive")
          employmentStatus  <- ask[EmploymentStatus]("aboutyou-employmentstatus").in[R] when !alive
          deceasedBefore  <- ask[Boolean]("aboutyou-deceasedbefore").in[R] when employmentStatus == Some(EmploymentStatus.Employed)
          notRequiredToComplete = deceasedBefore == Some(true)
          _ <- tell[Unit]("aboutyou-noloancharge")("_").in[R] when notRequiredToComplete
          id <- ask[Either[Nino,Utr]]("aboutyou-identity").in[R] when (!notRequiredToComplete)
          isCorrectPerson <- ask[String]("aboutyou-confirmation").validating(
            "name contains invalid characters", _.matches("^[a-zA-Z]+$")).in[R] when !id.isEmpty
        } yield {
          if (notRequiredToComplete)
            Left(NoNeedToComplete)
          else
            Right(Some(AboutYou(false, alive, id, deceasedBefore, employmentStatus, isCorrectPerson)))
        }
      }
  } yield (ret)

}
