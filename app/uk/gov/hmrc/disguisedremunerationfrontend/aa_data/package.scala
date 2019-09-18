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

import java.time.{LocalDate => Date, _}, format.DateTimeFormatter

import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.{TimeZone, ULocale}
import play.api.i18n.Messages

package object data {

  lazy val nonEmptyStringRegex = """^(?!\s*$).+"""

  type Nino = String
  type Utr = String
  type Paye = String
  type Money = BigDecimal
  type CaseRef = String
  type Year = Int
  type EndJourney = String

  implicit class HMRCDate(val date: Date) {

    def financialYear: Int = {
      val turningPoint = Date.of(date.getYear, 4, 6)
      if (date.isBefore(turningPoint))
        date.getYear - 1
      else
        date.getYear
    }
  }

  implicit class IntToFinancialYear(year: Int) {
    def toFinancialYear: (Date,Date) = (
      Date.of(year, 4, 6),
      Date.of(year+1, 4, 5)
    )
  }

  private val zone = "Europe/London"
  private val zoneId: ZoneId = ZoneId.of(zone)
  private val timeFomat = "h:mma"
  def formattedTimeNow: String = LocalDateTime.now(zoneId).format(DateTimeFormatter.ofPattern(timeFomat)).toLowerCase

  def formatDate(localDate: Date, dateFormatPattern: String = "d MMMM yyyy")(implicit messages: Messages):String = {
    val date = java.util.Date.from(localDate.atStartOfDay(zoneId).toInstant)
    createDateFormatForPattern(dateFormatPattern).format(date)
  }

  private def createDateFormatForPattern(pattern: String)(implicit messages: Messages): SimpleDateFormat = {
    val uLocale = new ULocale(messages.lang.code)
    val validLang: Boolean = ULocale.getAvailableLocales.contains(uLocale)
    val locale: ULocale = if (validLang) uLocale else ULocale.getDefault
    val sdf = new SimpleDateFormat(pattern, locale)
    sdf.setTimeZone(TimeZone.getTimeZone(zone))
    sdf
  }
}
