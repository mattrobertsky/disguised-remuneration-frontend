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

sealed trait YesNoUnknownWrittenOff
object YesNoUnknownWrittenOff {
  case class Yes(writtenOff: WrittenOff) extends YesNoUnknownWrittenOff
  val No = a.No
  object a {
    case object No extends YesNoUnknownWrittenOff
  }
  val Unknown = b.Unknown
  object b {
    case object Unknown extends YesNoUnknownWrittenOff
  }

  def apply(optString: List[String]): YesNoUnknownWrittenOff =
    optString match {
      case List(u) if u == YesNoUnknown.Unknown.entryName => YesNoUnknownWrittenOff.Unknown
      case List(n) if n == YesNoUnknown.No.entryName => YesNoUnknownWrittenOff.No
      case List(amount,tax) => YesNoUnknownWrittenOff.Yes(WrittenOff.fromList(List(amount,tax)))
      case err => throw new IllegalStateException(s"cannot parse $err")
    }

  def unapply(yesNoDoNotKnow: YesNoUnknownWrittenOff): Option[List[String]] =
    yesNoDoNotKnow match {
      case Unknown => Some(List(YesNoUnknown.Unknown.entryName))
      case No => Some(List(YesNoUnknown.No.entryName))
      case Yes(msg) => Some(msg.toList)
    }
}
