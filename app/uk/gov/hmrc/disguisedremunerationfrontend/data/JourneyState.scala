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
import play.api.libs.json.{JsPath, Reads, Writes}

case class JourneyState(
                         aboutYou: Option[AboutYou] = None,
                         schemes: List[Scheme] = Nil,
                         contactDetails: Option[ContactDetails] = None,
                         details: List[LoanDetails] = Nil
                       ) {
  def readyToSubmit = aboutYou.isDefined && contactDetails.isDefined && schemes.nonEmpty
  //&& detailsStatus.forall(_._3.isDefined)
}

object JourneyState {
  import AboutYou._
  import ContactDetails._
  import Scheme._
  import LoanDetails._

  implicit val journeyStateReads: Reads[JourneyState] = (
    (JsPath \ "aboutYou").readNullable[AboutYou] and
      (JsPath \ "schemes").read[List[Scheme]] and
      (JsPath \ "contactDetails").readNullable[ContactDetails] and
      (JsPath \ "details").read[List[LoanDetails]]
    )(JourneyState.apply _)

  implicit val journeyStateWrites: Writes[JourneyState] = (
    (JsPath \ "aboutYou").writeNullable[AboutYou] and
      (JsPath \ "schemes").write[List[Scheme]] and
      (JsPath \ "contactDetails").writeNullable[ContactDetails] and
      (JsPath \ "details").write[List[LoanDetails]]
    )(unlift(JourneyState.unapply))
}

