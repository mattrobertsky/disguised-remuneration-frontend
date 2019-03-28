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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

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

  implicit class IntToFinancialYear(year: Int) {
    def toFinancialYear: (Date,Date) = (
      java.time.LocalDate.of(year, 4, 6),
      java.time.LocalDate.of(year+1, 4, 5)
    )
  }

  def getDateTime(): String = {
    val now = LocalDateTime.now()
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val timeFormat = DateTimeFormatter.ofPattern("h:mm a")
    val dateTime = s"${now.format(dateFormat)} at ${now.format(timeFormat)}"
    dateTime
  }
}
