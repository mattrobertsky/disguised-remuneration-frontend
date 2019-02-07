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

package dr

import ltbs.uniform._
import org.atnos.eff._
import cats.implicits._

case class LoanDetails(
  scheme: Scheme, 
  year: Int,
  amount: Money,
  hmrcApproved: Boolean,
  genuinelyRepaid: Money,
  writtenOff: Option[WrittenOff]
)

object LoanDetails {

  type Stack = Fx.fx3[
    UniformAsk[Money,?],
    UniformAsk[Boolean,?],
    UniformAsk[Option[WrittenOff],?]
  ]

  def program[R
      : _uniform[Money,?]
      : _uniform[Boolean,?]
      : _uniform[Option[WrittenOff],?]
  ](scheme: Scheme, year: Int): Eff[R, LoanDetails] = for {
    amount          <- uask[R,Money]("amount")
    hmrcApproved    <- uask[R,Boolean]("hmrcApproved")
    genuinelyRepaid <- uask[R,Money]("genuinelyRepaid")
    writtenOff      <- uask[R,Option[WrittenOff]]("writtenOff")
  } yield LoanDetails(scheme, year,amount,hmrcApproved,genuinelyRepaid,writtenOff)
  
}
