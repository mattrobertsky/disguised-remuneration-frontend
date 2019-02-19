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
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration._

case class AboutYou(
  diedOn: Option[Date],
  identification: Either[Nino, Utr],
  employmentStatus: Option[EmploymentStatus]
)

object AboutYou {

  type Stack = Fx.fx4[
    UniformAsk[Boolean,?],
    UniformAsk[Option[Date],?],
    UniformAsk[Either[Nino,Utr],?],
    UniformAsk[Option[EmploymentStatus],?]
    ]

  def program[R
  : _uniform[Boolean,?]
  : _uniform[Option[Date],?]
  : _uniform[Either[Nino,Utr],?]
  : _uniform[Option[EmploymentStatus],?]
  ]: Eff[R, Option[AboutYou]] = {
    for {
      diedOn  <- uask[R,Option[Date]]("diedOn")
      id      <- uask[R,Either[Nino,Utr]]("identification")
      status <- uask[R,Option[EmploymentStatus]]("employmentStatus")
    } yield AboutYou(diedOn, id, status)
  } when uask[R,Boolean]("otherPerson")

}
