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

import play.api.libs.json._

object NinoOrUtr {
  implicit def eitherReads[Nino, Utr](implicit Nino: Reads[Nino], Utr: Reads[Utr] ): Reads[Either[Nino, Utr]] =
    Reads[Either[Nino, Utr]] { json =>
      Nino.reads(json) match {
        case JsSuccess(value, path) => JsSuccess(Left(value), path)
        case JsError(errNino) => Utr.reads(json) match {
          case JsSuccess(value, path) => JsSuccess(Right(value), path)
          case JsError(errUtr) => JsError(JsError.merge(errNino, errUtr))
        }
      }
    }

  implicit def eitherWrites[Nino, Utr](implicit Nino: Writes[Nino], Utr: Writes[Utr]): Writes[Either[Nino, Utr]] =
    Writes[Either[Nino,Utr]] {
      case Left(l) => Nino.writes(l)
      case Right(r) => Utr.writes(r)
    }

  implicit def eitherFormat[Nino,Utr](implicit Nino: Format[Nino], Utr: Format[Utr]): Format[Either[Nino,Utr]] =
    Format(eitherReads, eitherWrites)
}

