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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import cats.implicits._
import enumeratum.{Enum, EnumEntry, PlayJsonEnum}
import javax.inject.{Inject, Singleton}
import ltbs.uniform._
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.web.InferParser._
import ltbs.uniform.web.parser._
import ltbs.uniform.web._
import org.atnos.eff._
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.{Html, HtmlFormat}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.disguisedremunerationfrontend.actions.AuthorisedAction
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.AssetsFrontend.{optionHtml => _, _}
import uk.gov.hmrc.disguisedremunerationfrontend.data.JsonConversion._
import uk.gov.hmrc.disguisedremunerationfrontend.data._
import uk.gov.hmrc.disguisedremunerationfrontend.repo.ShortLivedStore
import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

sealed abstract class EmploymentStatus extends EnumEntry
object EmploymentStatus
    extends Enum[EmploymentStatus]
    with PlayJsonEnum[EmploymentStatus]
{
  val values = findValues
  case object Employed      extends EmploymentStatus
  case object SelfEmployed  extends EmploymentStatus
  case object Both          extends EmploymentStatus
}

// unable to move to data package!
// knownDirectSubclasses of YesNoDoNotKnow observed before subclass
// Yes registered
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
class JourneyController @Inject()(
  mcc: MessagesControllerComponents,
  auditConnector: AuditConnector,
  shortLivedStore: ShortLivedStore,
  authorisedAction: AuthorisedAction,
  val authConnector: AuthConnector
)(
  implicit val appConfig: AppConfig
) extends FrontendController(mcc)
    with PlayInterpreter
    with I18nSupport
    with AuthorisedFunctions
{

  var state: JourneyState = JourneyState()

  def messages( request: Request[AnyContent] ): UniformMessages[Html] =
    convertMessages(messagesApi.preferred(request)) |+| UniformMessages.bestGuess.map(HtmlFormat.escape)

  def renderForm(
    key: List[String],
    errors: ErrorTree,
    form: Html,
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messagesIn: ltbs.uniform.UniformMessages[Html]
  ): Html = {
    implicit val r = request
    val content = views.html.about_you(
      key.last,
      errors,
      form,
      breadcrumbs
    )(messagesIn, request)
    implicit val msg: UniformMessages[Html] = messages(request)
    views.html.main_template(title = "Send your loan charge details")(content)
  }

  override lazy val parse = super[FrontendController].parse

  def index: Action[AnyContent] = authorisedAction.async { implicit request =>
    implicit val msg: UniformMessages[Html] = messages(request)
    val content = views.html.main_template(
      title = "Send your loan charge details"
    )(views.html.index(state))

    Future.successful(Ok(content))
  }

  def contactDetails(implicit key: String) =
    authorisedAction.async { implicit request =>
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
        def render(
          key: String,
          values: Input,
          errors: ErrorTree,
          messages: UniformMessages[Html]
        ) = implicitly[HtmlField[String]].render(key, values, errors, messages)
      }

      runWeb(
        program = ContactDetails.program[FxAppend[Stack, PlayStack]](
          state.contactDetails
        ).useForm(automatic[Unit, Address])
          .useForm(automatic[Unit, TelAndEmail]),
        shortLivedStore.persistence(request.internalId)
      ){data =>
        state = state.copy(contactDetails = Some(data))
        Future.successful(Redirect(routes.JourneyController.index()))
      }
    }

  def addScheme(key: String) = runScheme(None, key)

  def editScheme(
    schemeIndex: Int,
    key: String
  ) = runScheme(Some(schemeIndex), key)

  def runScheme(
    schemeIndex: Option[Int],
    key: String
  ) = authorisedAction.async {
    implicit request =>

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
      shortLivedStore.persistence(request.internalId)
    ){data =>
      state = schemeIndex match {
        case Some(i) =>
          val updatedScheme = data.copy(
            loanDetailsProvided = default.fold(
              Map.empty[Year, LoanDetails]
            )(
              _.loanDetailsProvided
            )
          )
          state.copy(schemes = state.schemes.patch(i, Seq(updatedScheme), 1))
        case None    =>
          state.copy(schemes = data :: state.schemes)
      }
      Future.successful(Redirect(routes.JourneyController.index()))
    }
  }

  def loanDetails(
    schemeIndex: Int,
    year: Year,
    key: String
  ) = authorisedAction.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList
    import LoanDetails._
    val scheme = state.schemes(schemeIndex)
    val existing = scheme.loanDetails(year)
    runWeb(
      program = LoanDetails.program[FxAppend[Stack, PlayStack]](year, existing)
        .useForm(automatic[Unit, Money])
        .useForm(automatic[Unit, Boolean])
        .useForm(automatic[Unit, WrittenOff]),
      shortLivedStore.persistence(request.internalId)
    ){data =>
      val updatedScheme = scheme.copy(
        loanDetailsProvided = scheme.loanDetailsProvided + (year -> data))
      state = state.copy(
        schemes = state.schemes.patch(schemeIndex, Seq(updatedScheme), 1)
      )
      Future.successful(Redirect(routes.JourneyController.index()))
    }
  }

  def aboutYou(implicit key: String): Action[AnyContent] =
    authorisedAction.async { implicit request =>
      implicit val keys: List[String] = key.split("/").toList

      val customBool = {
        implicit val booleanField = new HtmlField[Boolean] {
          override def render(
            key: String,
            values: Input,
            errors: ErrorTree,
            messages: UniformMessages[Html]
          ): Html =
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
        shortLivedStore.persistence(request.internalId)
      ){
        _ match {
          case Left(err) =>
            Future.successful(Ok("TODO: Some action to be taken here"))
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

  def blocksFormState(
    implicit request: Request[AnyContent]
  ): List[(Html,List[(Html,Html)])] = {
    def msg(in: String): Html = messages(request)(in)
    import HtmlFormat.escape
    state match {
      case JourneyState(Some(aboutYou), schemes, Some(contactDetails)) =>
        val h: (Html,List[(Html,Html)]) = (
          msg("personal-details") -> List(
            msg("name") ->
              escape(usersNameFromGG),
            msg("filling-in-form-for-self") ->
              msg(if(aboutYou.isEmpty) "TRUE" else "FALSE"),
            msg("address") ->
              contactDetails.address.lines.
              map(escape).
              intercalate(Html("<br />")),
            msg("contact-details") -> {
              import contactDetails.telephoneAndEmail._
              List(
                telephone.map{x => ("telephone", x)},
                email.map{x => ("email-address", x)}
              ).flatten.map{ case (l,r) =>
                  msg(l) |+| Html(": ") |+| escape(r)
              }.intercalate(Html("<br />"))
            }
          )
        )

        val t: List[(Html,List[(Html,Html)])] = schemes.map { scheme =>
          import scheme._
          (msg("scheme") |+| escape(name)) -> List(
            msg("dates-you-received-loans") -> Html(""),
            msg("disclosure-of-tax-avoidance-schemes-dotas-number") ->
              dotasReferenceNumber.fold(msg("not-applicable"))(escape),
            msg("hmrc-case-reference-number") ->
              caseReferenceNumber.fold(msg("not-applicable")){escape},
            msg("employment-status") ->
              caseReferenceNumber
              .fold(msg("not-applicable")){_ => msg("Employed")},
            msg("loan-recipient") ->
              msg(if(loanRecipient) "TRUE" else "FALSE"),
            msg("tax-or-national-insurance-paid-or-agreed-to-pay") ->
              settlement.fold(msg("none")){x => Html(f"&pound;${x.amount}%,d")}
          )
        }
        h :: t
      case _ => Nil
    }
  }

  def cya = Action { implicit request =>
    implicit val m: UniformMessages[Html] = messages(request)
    val contents = views.html.cya(
      usersNameFromGG,
      blocksFormState,
      confirmationForm
    )

    Ok(views.html.main_template(
      title = "Check your answers before sending your details"
    )(contents))
  }

  def cyaPost = Action { implicit request =>
    implicit val m: UniformMessages[Html] = messages(request)
    confirmationForm.bindFromRequest.fold(
      formWithErrors => {
        val contents =
          views.html.cya(usersNameFromGG, blocksFormState, formWithErrors)
        BadRequest(views.html.main_template(
          title = "Check your answers before sending your details"
        )(contents))
      },
      postedForm => {
        auditConnector.sendExplicitAudit("disguisedRemunerationCheck", Json.toJson(state))
        val contents = views.html.confirmation(getDateTime())
        Ok(views.html.main_template(title = "Loan charge details received")(contents))
      }
    )
  }
}
