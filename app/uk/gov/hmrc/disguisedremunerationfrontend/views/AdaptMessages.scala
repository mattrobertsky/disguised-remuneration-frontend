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

package uk.gov.hmrc.disguisedremunerationfrontend.views

import java.util.Locale

import ltbs.uniform._
import play.api.Play
import play.api.i18n.Messages.Implicits._
import play.api.i18n.{Lang, Messages}
import play.api.mvc.RequestHeader
import play.twirl.api.Html

object AdaptMessages {

  implicit def ufMessagesToPlayMessages(implicit ufMessages: UniformMessages[Html], request: RequestHeader): Messages = new Messages {
    private val defaultLang = new Lang(Locale.ENGLISH)
    def lang: Lang = {
      Play.maybeApplication.map { implicit app =>
        val maybeLangFromCookie = request.cookies.get(Play.langCookieName).flatMap(c => Lang.get(c.value))
        maybeLangFromCookie.getOrElse(defaultLang)
      }.getOrElse(request.acceptLanguages.headOption.getOrElse(defaultLang))
    }
    def apply(key: String, args: Any*): String = ufMessages.apply(key, args:_*).toString
    def apply(keys: Seq[String], args: Any*): String = ufMessages.apply(keys.toList, args:_*).toString
    def translate(key: String, args: Seq[Any]): Option[String] = ufMessages.get(key, args:_*).map{_.toString}
    def isDefinedAt(key: String): Boolean = ufMessages.get(key).isDefined
    def asJava: play.i18n.Messages = ???
  }

}
