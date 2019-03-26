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
import enumeratum.{Enum, EnumEntry, PlayJsonEnum}
import javax.inject.{Inject, Singleton}

import ltbs.uniform._
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.web.HtmlField
import ltbs.uniform.web.InferParser._
import ltbs.uniform.web.parser._
import play.api.data.Form
import ltbs.uniform.web.{HtmlForm, Input, Messages, NoopMessages}
import ltbs.uniform.widgets.govuk._
import org.atnos.eff._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.Html
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration.{Date, Nino, Utr}
import uk.gov.hmrc.disguisedremunerationfrontend.data._
import uk.gov.hmrc.disguisedremunerationfrontend.data.render.RenderHtmlTemplate
import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import enumeratum.values._
import play.api.libs.json.Json
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.disguisedremunerationfrontend.actions.AuthorisedAction

sealed abstract class EmploymentStatus extends EnumEntry
object EmploymentStatus extends Enum[EmploymentStatus] with PlayJsonEnum[EmploymentStatus] {
  val values = findValues
  case object Employed      extends EmploymentStatus
  case object SelfEmployed  extends EmploymentStatus
  case object Both          extends EmploymentStatus
}

sealed trait YesNoDoNotKnow

object YesNoDoNotKnow {
  case class Yes(dotas: String) extends YesNoDoNotKnow
  case object No extends YesNoDoNotKnow

  def DoNotKnow = z.DoNotKnow
  object z {
    case object DoNotKnow extends YesNoDoNotKnow
  }
}


case class JourneyState(
  aboutYou: Option[Option[AboutYou]] = None,
  schemes: List[Scheme] = Nil,
  contactDetails: Option[ContactDetails] = None,
  details: List[LoanDetails] = Nil
) {
    def readyToSubmit: Boolean = aboutYou.isDefined && contactDetails.isDefined && schemes.nonEmpty
      //&& detailsStatus.forall(_._3.isDefined)
}

object JourneyState {
  //implicit val journeyStateFormatter: Format[JourneyState] = Json.format[JourneyState]
  implicit val journeyStateWrites = Json.writes[JourneyState]

}
@Singleton
class JourneyController @Inject()(mcc: MessagesControllerComponents, authorisedAction: AuthorisedAction, val authConnector: AuthConnector)(implicit val appConfig: AppConfig)
      extends FrontendController(mcc) with PlayInterpreter with I18nSupport with AuthorisedFunctions {

  var state: JourneyState = JourneyState()

  def messages( request: Request[AnyContent] ): ltbs.uniform.web.Messages = convertMessages(messagesApi.preferred(request))

  def listingPage[A](key: List[String],errors: ltbs.uniform.ErrorTree,elements: List[A],messages: ltbs.uniform.web.Messages)(implicit evidence$1: ltbs.uniform.web.Htmlable[A]): play.twirl.api.Html = ???

  def renderForm(key: List[String], errors: ErrorTree, form: Html, breadcrumbs: List[String], request: Request[AnyContent], messagesIn: ltbs.uniform.web.Messages): Html = {
    implicit val r = request
    views.html.main_template(title = "Send your loan charge details")(views.html.about_you(key.last, errors, form, "/" :: breadcrumbs)(messagesIn, request))
  }

  override lazy val parse = super[FrontendController].parse

  def index: Action[AnyContent] = authorisedAction.async { implicit request =>
      Future.successful(Ok(views.html.main_template(title = "Send your loan charge details")(views.html.index(state))))
  }


  def contactDetails(implicit key: String): Action[AnyContent] = authorisedAction.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList
    import ContactDetails._
      runWeb(
        program = ContactDetails.program[FxAppend[Stack, PlayStack]]
          .useForm(PlayForm.automatic[Unit, Address])
          .useForm(PlayForm.automatic[Unit, TelAndEmail]),
        MemoryPersistence
      ) { data =>
        state = state.copy(contactDetails = Some(data))
        Future.successful(Redirect(routes.JourneyController.index()))
    }
  }
  def addScheme(implicit key: String): Action[AnyContent] = authorisedAction.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList
    import Scheme._
      runWeb(
        program = Scheme.program[FxAppend[Stack, PlayStack]]
          .useForm(PlayForm.automatic[Unit, String])
          .useForm(PlayForm.automatic[Unit, Option[String]])
          .useForm(PlayForm.automatic[Unit, Option[Employer]])
          .useForm(PlayForm.automatic[Unit, TaxSettlement])
          .useForm(PlayForm.automatic[Unit, YesNoDoNotKnow])
          .useForm(PlayForm.automatic[Unit, Boolean])
          .useForm(PlayForm.automatic[Unit, Date])
          .useForm(PlayForm.automatic[Unit, (Date, Date)]),
        MemoryPersistence
      ) { data =>
        state = state.copy(schemes = data.get :: state.schemes) // remove get
        Future.successful(Redirect(routes.JourneyController.index()))
    }
  }

  implicit def renderTell: (Unit, String) => Html = {case _ => Html("")}

  def aboutYou(implicit key: String): Action[AnyContent] = authorisedAction.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList

    val customBool = {
      implicit val booleanField = new HtmlField[Boolean] {
        override def render( key: String, values: Input, errors: ErrorTree, messages: Messages ): Html =
          html.radios(
            key,
            Seq("FALSE","TRUE"),
            values.value.headOption,
            errors,
            messages
          )
      }
      PlayForm.automatic[Unit, Boolean]
    }

    val customNinoUTR = {
      implicit val booleanField = new HtmlField[Either[Nino,Utr]] {
        override def render( key: String, values: Input, errors: ErrorTree, messages: Messages ): Html =
          Html(RenderHtmlTemplate.generateIdentityHtml(messages))
      }
      PlayForm.automatic[Unit, Either[Nino,Utr]]
    }
    import AboutYou._
      runWeb(
        program = AboutYou.program[FxAppend[Stack, PlayStack]]
          .useFormMap {
            case List("aboutyou-completedby") => customBool
            case _ => PlayForm.automatic[Unit, Boolean]
          }
          .useForm(customNinoUTR)
          .useForm(PlayForm.automatic[Unit, EmploymentStatus])
          .useForm(PlayForm.automatic[Unit, String])
          .useForm(PlayForm.automatic[Unit, Unit]),
        MemoryPersistence
      ) {
        _ match {
          case Left(err) => Future.successful(Redirect(routes.HelloWorld.helloWorld()))
          //throw new RuntimeException("logout")
          case Right(data) =>
            state = state.copy(aboutYou = Some(data))
            Future.successful(Redirect(routes.JourneyController.index()))
      }
    }
  }
}

import java.util.concurrent.atomic._
object MemoryPersistence extends Persistence {
  private val storage = new AtomicReference(Map.empty[List[String], String])

  override def dataGet: Future[DB] = {
    Future.successful(storage.get())
  }

  override def dataPut(dataIn: DB): Future[Unit] = {
    storage.set(dataIn)
    Future.successful(Unit)
  }
}
