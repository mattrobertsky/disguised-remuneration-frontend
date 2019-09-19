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

import enumeratum._

sealed abstract class EmploymentStatus extends EnumEntry
object EmploymentStatus
    extends Enum[EmploymentStatus]
    with PlayJsonEnum[EmploymentStatus] {
  val values = findValues
  case object Employed extends EmploymentStatus
  case object SelfEmployed extends EmploymentStatus
  case object Both extends EmploymentStatus
}
