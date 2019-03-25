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
import ltbs.uniform.web._
import ltbs.uniform.web.{HtmlField, DataParser}
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
import uk.gov.hmrc.disguisedremunerationfrontend.data.disguisedremuneration.{Date, Nino, Utr}
import uk.gov.hmrc.disguisedremunerationfrontend.data._
import uk.gov.hmrc.disguisedremunerationfrontend.data.render.RenderHtmlTemplate
import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import enumeratum.values._
import play.api.libs.json._
import play.api.libs.json.Json
import AssetsFrontend.{optionHtml => _, _}
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import uk.gov.hmrc.disguisedremunerationfrontend.repo.SessionStore

sealed abstract class EmploymentStatus extends EnumEntry
object EmploymentStatus extends Enum[EmploymentStatus] with PlayJsonEnum[EmploymentStatus] {
  val values = findValues
  case object Employed      extends EmploymentStatus
  case object SelfEmployed  extends EmploymentStatus
  case object Both          extends EmploymentStatus
}

// unable to move to data package!
// knownDirectSubclasses of YesNoDoNotKnow observed before subclass Yes registered
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
  contactDetails: Option[ContactDetails] = None
) {
    def readyToSubmit = aboutYou.isDefined && contactDetails.isDefined && schemes.nonEmpty
      //&& detailsStatus.forall(_._3.isDefined)
}

object JourneyState {
  //implicit val journeyStateFormatter: Format[JourneyState] = Json.format[JourneyState]
  implicit val journeyStateWrites = Json.writes[JourneyState]

}
@Singleton
class JourneyController @Inject()(mcc: MessagesControllerComponents)(implicit val appConfig: AppConfig)
      extends FrontendController(mcc) with PlayInterpreter with I18nSupport {

  var state: JourneyState = JourneyState(schemes = List(
    Scheme(
      name="Cavalier Finance WanglePlus Account",
      dotasReferenceNumber=None,
      caseReferenceNumber=None,
      schemeStart=java.time.LocalDate.of(2015,1,1),
      schemeStopped=None,
      employee=None,
      loanRecipient=true,
      loanRecipientName=None,
      settlement=None,
      loanDetailsProvided=Map(2016 -> LoanDetails(1000, true, 100, None)
      )
    ),
    Scheme(
      name="Banco Desonesto CartaoCorrupto",
      dotasReferenceNumber=None,
      caseReferenceNumber=None,
      schemeStart=java.time.LocalDate.of(2017,1,1),
      schemeStopped=None,
      employee=None,
      loanRecipient=true,
      loanRecipientName=None,
      settlement=None,
      loanDetailsProvided=Map(2016 -> LoanDetails(1000, true, 100, None)
      )
    )
  ))

  def messages( request: Request[AnyContent] ): ltbs.uniform.web.Messages = convertMessages(messagesApi.preferred(request))

  def listingPage[A](key: List[String],errors: ltbs.uniform.ErrorTree,elements: List[A],messages: ltbs.uniform.web.Messages)(implicit evidence$1: ltbs.uniform.web.Htmlable[A]): play.twirl.api.Html = ???

  def renderForm(key: List[String], errors: ErrorTree, form: Html, breadcrumbs: List[String], request: Request[AnyContent], messagesIn: ltbs.uniform.web.Messages): Html = {
    implicit val r = request
    views.html.main_template(title = "Send your loan charge details")(views.html.about_you(key.last, errors, form, breadcrumbs)(messagesIn, request))
  }

  override lazy val parse = super[FrontendController].parse

  def index = Action { implicit request =>
    Ok(views.html.main_template(title = "Send your loan charge details")(views.html.index(state)))
  }

  def cacheWrite(sessionId: String, journeyState: JourneyState)(implicit request: Request[AnyContent]) =
    cache.store[JourneyState](sessionId, "drCache", journeyState)

  def contactDetails(implicit key: String) = Action.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList
    import ContactDetails._

    // tell uniform to not use the nested fields with radio buttons
    // for an Option[String], instead treat an empty string as None
    implicit val optStringParser = new DataParser[Option[String]] {

      import cats.implicits._

      def bind(in: Input): Either[ErrorTree,Option[String]] = in.value match {
        case Nil => Tree("required").asLeft
        case empty::Nil if empty.trim == "" => none[String].asRight
        case s::Nil => Some(s).asRight
        case _ => Tree("badValue").asLeft
      }

      def unbind(a: Option[String]): Input = Tree(List(a.getOrElse("")))
    }

    // tell uniform to use the same renderer for an Option[String] as
    // is used for a String field
    implicit val optStringHtml = new HtmlField[Option[String]] {
      def render(key: String, values: Input, errors: ErrorTree, messages: Messages) =
        implicitly[HtmlField[String]].render(key, values, errors, messages)
    }

    runWeb(
      program = ContactDetails.program[FxAppend[Stack, PlayStack]](state.contactDetails)
        .useForm(automatic[Unit, Address])
        .useForm(automatic[Unit, TelAndEmail]),
      MemoryPersistence
    ){data =>
      state = state.copy(contactDetails = Some(data))
      val uuid = cache.sessionUuid.getOrElse("dr-sessionId1")
      //what do we need here??+ ("uuid" -> java.util.UUID.randomUUID.toString)
      cacheWrite(uuid, state)
      Future.successful(Redirect(routes.JourneyController.index()))
    }

  }

  def addScheme(key: String) = runScheme(None, key)

  def editScheme(schemeIndex: Int, key: String) = runScheme(Some(schemeIndex), key)  

  def runScheme(schemeIndex: Option[Int], key: String) = Action.async { implicit request =>

    val default: Option[Scheme] = schemeIndex.map(state.schemes(_))
    implicit val keys: List[String] = key.split("/").toList
    import AssetsFrontend.optionHtml
    import Scheme._
    runWeb(
      program = Scheme.program[FxAppend[Stack, PlayStack]](default)
        .useForm(automatic[Unit, String])
        .useForm(automatic[Unit, Option[String]])
        .useForm(automatic[Unit, Option[Employer]])
        .useForm(automatic[Unit, TaxSettlement])
        .useForm(automatic[Unit,YesNoDoNotKnow])
        .useForm(automatic[Unit, Boolean])
        .useForm(automatic[Unit, Date])
        .useForm(automatic[Unit, (Date, Date)]),
      MemoryPersistence
    ){data =>
      state = schemeIndex match {
        case Some(i) =>
          val updatedScheme = data.copy(loanDetailsProvided = default.fold(Map.empty[Year, LoanDetails])(_.loanDetailsProvided))
          state.copy(schemes = state.schemes.patch(i, Seq(updatedScheme), 1))
        case None    => 
          state.copy(schemes = data :: state.schemes)
      }
      Future.successful(Redirect(routes.JourneyController.index()))
    }
  }

  def loanDetails(schemeIndex: Int, year: Year, key: String) = Action.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList
    import LoanDetails._
    val scheme = state.schemes(schemeIndex)
    val existing = scheme.loanDetails(year)
    runWeb(
      program = LoanDetails.program[FxAppend[Stack, PlayStack]](existing)
        .useForm(automatic[Unit, Money])
        .useForm(automatic[Unit, Boolean])
        .useForm(automatic[Unit, WrittenOff]),
      MemoryPersistence
    ){data =>
      val updatedScheme = scheme.copy(
        loanDetailsProvided = scheme.loanDetailsProvided + (year -> data))
      state = state.copy(schemes = state.schemes.patch(schemeIndex, Seq(updatedScheme), 1))
      Future.successful(Redirect(routes.JourneyController.index()))
    }
  }

  implicit def renderTell: (Unit, String) => Html = {case _ => Html("")}

  def aboutYou(implicit key: String) = Action.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList

    val customBool = {
      implicit val booleanField = new HtmlField[Boolean] {
        override def render( key: String, values: Input, errors: ErrorTree, messages: Messages ): Html =
          views.html.uniform.radios(
            key,
            Seq("FALSE","TRUE"),
            values.value.headOption,
            errors,
            messages
          )
      }
      automatic[Unit, Boolean]
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
      program = AboutYou.program[FxAppend[Stack, PlayStack]](state.aboutYou)
        .useFormMap{
          case List("aboutyou-completedby") => customBool
          case _ => automatic[Unit,Boolean]
        }
        .useForm(automatic[Unit, Either[Nino,Utr]])
        .useForm(automatic[Unit,EmploymentStatus])
        .useForm(automatic[Unit,String])
        .useForm(automatic[Unit, Unit]),
      MemoryPersistence
    ){
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
