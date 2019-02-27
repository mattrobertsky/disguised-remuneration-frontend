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

import cats.data.Validated
import enumeratum.{Enum, EnumEntry}
import javax.inject.{Inject, Singleton}
import ltbs.uniform.ErrorTree
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.web.HtmlField
import ltbs.uniform.web.InferParser._
import ltbs.uniform.web.parser._
import play.api.data.Form
import ltbs.uniform.web.{HtmlForm, Input, Messages, NoopMessages}
import ltbs.uniform.widgets.govuk._
import org.atnos.eff._
import play.api.i18n.I18nSupport
import play.api.mvc.{AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.Html
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
//import uk.gov.hmrc.disguisedremunerationfrontend.data._
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration.{Nino, Utr}
//import uk.gov.hmrc.disguisedremunerationfrontend.data.AboutYou._
//import uk.gov.hmrc.disguisedremunerationfrontend.data.Scheme._
import uk.gov.hmrc.disguisedremunerationfrontend.data._
//import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration._
import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

sealed abstract class EmploymentStatus extends EnumEntry
object EmploymentStatus extends Enum[EmploymentStatus] {
  val values = findValues
  case object Employed      extends EmploymentStatus
  case object SelfEmployed  extends EmploymentStatus
  case object Both          extends EmploymentStatus
}

case class JourneyState(
  aboutYou: Option[Option[AboutYou]] = None,
  schemes: List[Scheme] = Nil,
  contactDetails: Option[ContactDetails] = None,
  details: List[LoanDetails] = Nil
) {
    def readyToSubmit = aboutYou.isDefined
    // && contactDetails.isDefined &&
    //schemes.nonEmpty && detailsStatus.forall(_._3.isDefined)
}

@Singleton
class JourneyController @Inject()(mcc: MessagesControllerComponents)(implicit val appConfig: AppConfig)
      extends FrontendController(mcc) with PlayInterpreter with I18nSupport {

  var state: JourneyState = JourneyState()

  def messages( request: Request[AnyContent] ): ltbs.uniform.web.Messages = convertMessages(messagesApi.preferred(request))

  def renderForm(key: String, errors: ErrorTree, form: Html, breadcrumbs: List[String], request: Request[AnyContent], messagesIn: ltbs.uniform.web.Messages): Html = {
    views.html.chrome(key, errors, form, "/" :: breadcrumbs)(messagesIn, request)
  }

  override lazy val parse = super[FrontendController].parse

  def index = Action { implicit request =>
    Ok(views.html.main_template(title = "Send your loan charge details")(views.html.index(state)))
  }

  def addScheme(implicit key: String) = Action.async { implicit request => ???
//    import Scheme._
//    runWeb(
//      program = Scheme.program[FxAppend[Stack, PlayStack]]
//        .useForm(PlayForm.automatic[String]),
//      MemoryPersistence
//    ){data =>
//      state = state.copy(schemes = data :: state.schemes)
//      Future.successful(Ok(data.toString))
//      Future.successful(Ok("Scheme"))
//    }
  }

  // How do I have 2 or more of these?
  implicit val booleanField = new HtmlField[Boolean] {
    def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
      html.radios(
        key,
        Seq("FALSE","TRUE"),
        values.value.headOption,
        errors,
        messages
      )
  }



  def aboutYou(implicit key: String) = Action.async { implicit request =>
    import AboutYou._
    runWeb(
      program = AboutYou.program[FxAppend[Stack, PlayStack]]
        .useForm(PlayForm.automatic[Boolean])
        .useForm(PlayForm.automatic[Either[Nino,Utr]])
        .useForm(PlayForm.automatic[EmploymentStatus])
        .useForm(PlayForm.automatic[Unit]),
      MemoryPersistence
    ){data =>
      state = state.copy(aboutYou = Some(data))
      Future.successful(Redirect(routes.JourneyController.index()))
    }
  }

}

import java.util.concurrent.atomic._
object MemoryPersistence extends Persistence {
  private val storage = new AtomicReference(Map.empty[String, String])

  override def dataGet: Future[DB] = {
    Future.successful(storage.get())
  }

  override def dataPut(dataIn: DB): Future[Unit] = {
    storage.set(dataIn)
    Future.successful(Unit)
  }
}