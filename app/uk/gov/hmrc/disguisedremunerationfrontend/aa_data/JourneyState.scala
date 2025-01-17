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

case class JourneyState(
  aboutYou: Option[AboutYou] = None,
  schemes: List[Scheme] = Nil,
  contactDetails: Option[ContactDetails] = None
) {
  def readyToSubmit =
    aboutYou.isDefined &&
    contactDetails.isDefined &&
    schemes.nonEmpty &&
    schemes.forall(_.readyToSubmit)
}
