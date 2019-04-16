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

object JsonConversion {
  implicit private[data] lazy val employerFormatter = Json.format[Employer]
  implicit private[data] lazy val writtedOffFormatter = Json.format[WrittenOff]
  implicit private[data] lazy val addressFormatter = Json.format[Address]
  implicit private[data] lazy val telAndEmailFormatter = Json.format[TelAndEmail]
  implicit private[data] lazy val contactDetailsFormatter = Json.format[ContactDetails]
  implicit private[data] lazy val loanDetailsFormatter = Json.format[LoanDetails]
  implicit private[data] lazy val taxSettlementFormatter = Json.format[TaxSettlement]

  implicit private[data] def intMapFormatter[A: Format] = new Format[Map[Int,A]] {
    // TODO: Error handling
    def reads(json: JsValue): JsResult[Map[Int,A]] = {
      val obj = json.as[Map[String, JsObject]].map {
        case (intString,valueObj) =>
          (Integer.parseInt(intString), valueObj.as[A])
      }
      JsSuccess(obj)
    }

    def writes(o: Map[Int,A]): JsValue = JsObject(
      o.map{ case (keyInt, value) =>
        (keyInt.toString, Json.toJson(value))
      }
    )
  }

  implicit private[data] lazy val schemeFormatter: Format[Scheme] = Json.format[Scheme]

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

    implicit lazy val ef = eitherFormatter[Nino,Utr]("nino", "utr")
    lazy val innerF = Json.format[AboutYou]

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

  case class FlatState(
    submissionId: String,
    username: String,
    aboutYou: Option[Option[AboutYou]],
    schemeName: String,
    dotasReferenceNumber: Option[String],
    caseReferenceNumber: Option[String],
    schemeStart: Date,
    schemeStopped: Option[Date],
    employee: Option[Employer],
    loanRecipient: Boolean,
    loanRecipientName: Option[String],
    settlement: Option[TaxSettlement],
    year: Int,
    hmrcApproved: Boolean,
    amount: Money,
    genuinelyRepaid: Option[Money],
    writtenOff: Option[WrittenOff],
    contactDetails: Option[ContactDetails]
  )
  case object FlatState {
    implicit val format = Json.format[FlatState]
  }



}
