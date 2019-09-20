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

package uk.gov.hmrc.disguisedremunerationfrontend.controllers

import javax.inject.Inject
import play.api.Environment
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.disguisedremunerationfrontend.views.html.accessibility_statement
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import play.api.Configuration
import ltbs.uniform.UniformMessages
import play.twirl.api.Html
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig

class AccessibilityStatementController @Inject()(
  messagesControllerComponents: MessagesControllerComponents,
  environment: Environment
)(
  implicit val configuration: Configuration,
  implicit val appConfig: AppConfig
) extends FrontendController(messagesControllerComponents) with I18nSupport {

  lazy val interpreter = DRInterpreter(appConfig, this, messagesApi)

  def showAccessibilityStatement: Action[AnyContent] = Action { implicit request =>
    implicit val msg: UniformMessages[Html] = interpreter.messages(request)
    Ok(accessibility_statement())
  }


}
