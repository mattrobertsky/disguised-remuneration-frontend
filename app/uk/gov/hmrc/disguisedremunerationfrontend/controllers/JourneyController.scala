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

import enumeratum.{Enum, EnumEntry, PlayJsonEnum}
import javax.inject.{Inject, Singleton}
import ltbs.uniform._
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.web.InferParser._
import ltbs.uniform.web.parser._
import ltbs.uniform.web.{DataParser, HtmlField, Input, Messages}
import org.atnos.eff._
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.I18nSupport
import play.api.libs.json._
import play.api.mvc.{AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.Html
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.AssetsFrontend.{optionHtml => _, _}
import uk.gov.hmrc.disguisedremunerationfrontend.data.JsonConversion._
import uk.gov.hmrc.disguisedremunerationfrontend.data.{Date, Nino, Utr, _}
import uk.gov.hmrc.disguisedremunerationfrontend.repo.ShortLivedStore
import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

  val DoNotKnow = z.DoNotKnow
  object z {
    case object DoNotKnow extends YesNoDoNotKnow
  }
}

@Singleton
class JourneyController @Inject()(mcc: MessagesControllerComponents, auditConnector: AuditConnector, shortLivedStore: ShortLivedStore)( implicit val appConfig: AppConfig)
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

  def messages( request: Request[AnyContent] ): ltbs.uniform.web.Messages =
    convertMessages(messagesApi.preferred(request))

  def renderForm(key: List[String], errors: ErrorTree, form: Html, breadcrumbs: List[String], request: Request[AnyContent], messagesIn: ltbs.uniform.web.Messages): Html = {
    implicit val r = request
    views.html.main_template(title = "Send your loan charge details")(views.html.about_you(key.last, errors, form, breadcrumbs)(messagesIn, request))
  }

  override lazy val parse = super[FrontendController].parse

  def index = Action { implicit request =>
    Ok(views.html.main_template(title = "Send your loan charge details")(views.html.index(state)))
  }

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
      shortLivedStore.persistence("todo-userId") // TODO get the userId from auth
    ){data =>
      state = state.copy(contactDetails = Some(data))
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
      shortLivedStore.persistence("todo-userId") // TODO get the userId from auth
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
      shortLivedStore.persistence("todo-userId") // TODO get the userId from auth
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
      shortLivedStore.persistence("todo-userId") // TODO get the userId from auth
    ){
      _ match {
      case Left(err) => Future.successful(Redirect(routes.HelloWorld.helloWorld()))
                        //throw new RuntimeException("logout")
      case Right(data: Option[AboutYou]) =>
        state = state.copy(aboutYou = Some(data))
        Future.successful(Redirect(routes.JourneyController.index()))
      }
    }
  }

  val confirmationForm = Form(single(
    "confirm" -> boolean.verifying("error.cya.confirmation-needed", identity(_))
  ))

  val usersNameFromGG = "Joe Bloggs"

  def blocksFormState: List[(String,List[(String,Html)])] = state match {
    case JourneyState(Some(aboutYou), schemes, Some(contactDetails)) =>
      (
        "Personal Details" -> List(
          "Name" -> Html(usersNameFromGG),
          "Filling in form for self" -> Html(if(aboutYou.isEmpty) "Yes" else "No"),
          "Address" -> Html(contactDetails.address.lines.mkString("<br />")),
          "Contact Details" -> Html{
            import contactDetails.telephoneAndEmail._
            List(
              telephone.map{x => "Telephone: " + x},
              email.map{x => "Email address: " + x}
            ).flatten.mkString("<br />")
          }
        )
      ) :: schemes.map { scheme =>
        import scheme._
        ("Scheme: " + name) -> List(
          "Dates you received loans" -> Html(""),
          "Disclosure of Tax Avoidance Schemes (DOTAS) number" -> Html(dotasReferenceNumber.fold("n/a"){identity}),
          "HMRC case reference number" -> Html(caseReferenceNumber.fold("n/a"){identity}),
          "Employment status" -> Html(caseReferenceNumber.fold("n/a"){_ => "Employed"}),
          "Loan recipient" -> Html(if(loanRecipient) "Yes" else "No"),
          "Tax or National Insurance paid or agreed to pay" -> Html(settlement.fold("None"){x => f"&pound;${x.amount}%,d"})
        )
      }
    case _ => Nil
  }

  def cya = Action { implicit request =>
    implicit val m: Messages = messages(request)
    val contents = views.html.cya(usersNameFromGG, blocksFormState, confirmationForm)
    Ok(views.html.main_template(title = "Check your answers before sending your details")(contents))
  }

  def sendToSplunk(State: JourneyState)(implicit hc: HeaderCarrier): Unit = {
    val auditSource = "disguised-remuneration"
    val auditType = "disguisedRemunerationCheck"
    auditConnector.sendExplicitAudit(auditSource, Json.toJson(state))
  }

  def cyaPost = Action { implicit request =>
    confirmationForm.bindFromRequest.fold(
      formWithErrors => {
        implicit val m: Messages = messages(request)
        val contents = views.html.cya(usersNameFromGG, blocksFormState, formWithErrors)
        Ok(views.html.main_template(title = "Check your answers before sending your details")(contents))
      },
      postedForm => {
        sendToSplunk(state)
        Ok(Json.toJson(state))  // TODO: Need to add confirmation page
      }
    )
  }
}
