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

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class JourneyState(
                         aboutYou: Option[Option[AboutYou]] = None,
                         schemes: List[Scheme] = Nil,
                         contactDetails: Option[ContactDetails] = None
                       ) {
  def readyToSubmit = aboutYou.isDefined && contactDetails.isDefined && schemes.nonEmpty
  //&& detailsStatus.forall(_._3.isDefined)
}

object JourneyState {

  private[data] def eitherFormatter[A: Format,B: Format](
    leftName: String = "left",
    rightName: String = "right"
  ): Format[Either[A,B]] = new Format[Either[A,B]] {
    def reads(json: JsValue): JsResult[Either[A,B]] = json match {
      case JsObject(left) if left.isDefinedAt(leftName) =>
        implicitly[Format[A]].reads(left(leftName)).map{x => Left(x)}
      case JsObject(right) if right.isDefinedAt(rightName) =>
        implicitly[Format[B]].reads(right(rightName)).map{x => Right(x)}
      case _ => JsError(s"cannot find $leftName or $rightName")
    }
    def writes(o: Either[A,B]): JsValue = o match {
      case Left(a) => Json.obj(leftName -> a)
      case Right(b) => Json.obj(rightName -> b)
    }
  }

  implicit private[data] def formatAboutYouOptionOption = new Format[Option[AboutYou]] {

    implicit val ef = eitherFormatter[Nino,Utr]("nino", "utr")
    val innerF = Json.format[AboutYou]

    def reads(json: JsValue): JsResult[Option[AboutYou]] = json match {
      case JsString("completedBySelf") => JsSuccess(None)
      case x => innerF.reads(x).map{Some(_)}
    }

    def writes(o: Option[AboutYou]): JsValue = o match {
      case None => JsString("completedBySelf")
      case Some(obj) => innerF.writes(obj)
    }
  }


  implicit def journeyStateFormat: Format[JourneyState] = {

    (
      (JsPath \ "aboutYou").formatNullable[Option[AboutYou]] and
        (JsPath \ "schemes").format[List[Scheme]] and
        (JsPath \ "contactDetails").formatNullable[ContactDetails]
    )(JourneyState.apply, unlift(JourneyState.unapply))
  }
}
