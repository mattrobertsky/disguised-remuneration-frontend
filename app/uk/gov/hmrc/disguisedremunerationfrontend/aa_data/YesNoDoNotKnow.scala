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

sealed trait YesNoDoNotKnow

object YesNoDoNotKnow {
  case class Yes(dotas: String) extends YesNoDoNotKnow
  val No = y.No
  object y {
    case object No extends YesNoDoNotKnow
  }
  val DoNotKnow = z.DoNotKnow
  object z {
    case object DoNotKnow extends YesNoDoNotKnow
  }

  def apply(optString: String): YesNoDoNotKnow = optString match {
    case u if u == YesNoUnknown.CUnknown.entryName => DoNotKnow
    case n if n == YesNoUnknown.BNo.entryName      => No
    case msg                                       => Yes(msg)
  }

  def unapply(yesNoDoNotKnow: YesNoDoNotKnow): Option[String] =
    yesNoDoNotKnow match {
      case DoNotKnow => Some(YesNoUnknown.CUnknown.entryName)
      case No => Some(YesNoUnknown.BNo.entryName)
      case Yes(msg) => Some(msg)
    }
}
