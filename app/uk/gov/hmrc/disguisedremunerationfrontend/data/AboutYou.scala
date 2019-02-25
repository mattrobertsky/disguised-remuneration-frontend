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
  employmentStatus: Option[EmploymentStatus] = None
)

object AboutYou {

  type Stack = Fx.fx4[
    UniformAsk[Boolean,?],
    UniformAsk[Unit,?],
    UniformAsk[Either[Nino,Utr],?],
    UniformAsk[EmploymentStatus,?]
  ]

  def program[R
  : _uniform[Boolean,?]
  : _uniform[Either[Nino,Utr],?]
  : _uniform[EmploymentStatus,?]
  : _uniform[Unit,?]
  ]: Eff[R, Option[AboutYou]] = {
    for {
      alive   <- uask[R, Boolean]("aboutyou-personalive")
      employmentStatus  <- uask[R,EmploymentStatus]("aboutyou-employmentstatus") when !alive
      deceasedBefore  <- uask[R,Boolean]("aboutyou-deceasedbefore") when employmentStatus == Some(EmploymentStatus.Employed)
      notRequiredToComplete <-  uask[R, Unit]("aboutyou-noloancharge") when deceasedBefore == Some(true)
      id <- uask[R, Either[Nino,Utr]]("aboutyou-identity") when notRequiredToComplete.isEmpty
      isCorrectPerson <- uask[R, Boolean]("aboutyou-confirmation") when !id.isEmpty
    } yield {
      AboutYou(false, alive, id, deceasedBefore, employmentStatus)
    }

  }  when uask[R, Boolean]("aboutyou-completedby")

}
