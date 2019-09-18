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

package uk.gov.hmrc.disguisedremunerationfrontend.config

import javax.inject.{Inject, Singleton}
import ltbs.uniform.UniformMessages
import play.api.i18n
import play.api.i18n.MessagesApi
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.play.bootstrap.http.FrontendErrorHandler

@Singleton
class ErrorHandler @Inject()(val messagesApi: MessagesApi, implicit val appConfig: AppConfig) extends FrontendErrorHandler {

  override def standardErrorTemplate(pageTitle: String, heading: String, message: String)(implicit request: Request[_]): Html = {
    implicit val messages: UniformMessages[Html] = convertMessages(messagesApi.preferred(request))
    views.html.error_template(pageTitle, heading, message)
  }

  // this is here temporarily, see https://github.com/ltbs/uniform-scala/issues/73
  def convertMessages(input: i18n.Messages, escapeHtml: Boolean = false): UniformMessages[Html] = {
    val stringMessages = new UniformMessages[String]{
      override def apply(key: List[String],args: Any*): String = {
        input(key, args:_*)
      }
      override def apply(key: String,args: Any*): String = {
        input(key, args:_*)
      }
      def get(key: String,args: Any*): Option[String] = if (input.isDefinedAt(key))
        Some(input.messages(key, args:_*))
      else
        None

      override def get(key: List[String],args: Any*): Option[String] = key collectFirst {
        case k if input.isDefinedAt(k) => input.messages(k, args:_*)
      }

      def list(key: String,args: Any*): List[String] = {
        @annotation.tailrec
        def inner(cnt: Int = 2, acc: List[String] = Nil): List[String] =
          get(s"$key.$cnt", args:_*) match {
            case Some(m) => inner(cnt+1, m :: acc)
            case None    => acc
          }

        List(key, s"$key.1").map(get(_, args:_*)).flatten ++ inner().reverse
      }
    }
    if (escapeHtml) stringMessages.map(
      play.twirl.api.HtmlFormat.escape
    ) else
      stringMessages.map(Html.apply)
  }

}
