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

sealed abstract class YesNoUnknown extends EnumEntry
object YesNoUnknown
  extends Enum[YesNoUnknown]
    with PlayJsonEnum[YesNoUnknown]
{
  val values = findValues
  case object AYes      extends YesNoUnknown {
    override def entryName: String = "Yes"
  }
  case object BNo       extends YesNoUnknown {
    override def entryName: String = "No"
  }
  case object CUnknown  extends YesNoUnknown {
    override def entryName: String = "Unknown"
  }
}
