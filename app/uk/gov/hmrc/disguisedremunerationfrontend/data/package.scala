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

package uk.gov.hmrc.disguisedremunerationfrontend

package object data {
  type Nino = String
  type Utr = String
  type Paye = String
  type Money = Int
  type Date = java.time.LocalDate
  type Year = Int
  type EndJourney = String

  implicit class HMRCDate(val date: Date) {

    def financialYear: Int = {
      val turningPoint = java.time.LocalDate.of(date.getYear, 4, 6)
      if (date.isBefore(turningPoint))
        date.getYear - 1
      else
        date.getYear
    }
  }
}
