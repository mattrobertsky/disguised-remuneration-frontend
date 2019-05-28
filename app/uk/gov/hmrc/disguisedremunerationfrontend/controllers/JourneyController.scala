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

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.implicits._
import enumeratum.{Enum, EnumEntry, PlayJsonEnum}
import javax.inject.{Inject, Singleton}
import ltbs.uniform._
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.web.InferParser._
import ltbs.uniform.web._
import ltbs.uniform.web.parser._
import org.atnos.eff._
import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.I18nSupport
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.{Html, HtmlFormat}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.disguisedremunerationfrontend.actions.{AuthorisedAction, AuthorisedRequest}
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.AssetsFrontend.{optionHtml => _,  _}
import uk.gov.hmrc.disguisedremunerationfrontend.data.JsonConversion.{FlatState, journeyStateFormat}
import uk.gov.hmrc.disguisedremunerationfrontend.data.{Date, Nino, Utr, _}
import uk.gov.hmrc.disguisedremunerationfrontend.repo.{JourneyStateStore, ShortLivedStore}
import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

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

sealed abstract class YesNoUnknown extends EnumEntry
object YesNoUnknown
  extends Enum[YesNoUnknown]
    with PlayJsonEnum[YesNoUnknown]
{
  val values = findValues
  case object Yes      extends YesNoUnknown
  case object No       extends YesNoUnknown
  case object Unknown  extends YesNoUnknown
}

// unable to move to data package!
// knownDirectSubclasses of YesNoDoNotKnow observed before subclass
// Yes registered
sealed trait YesNoDoNotKnow

object YesNoDoNotKnow {
  case class Yes(dotas: String) extends YesNoDoNotKnow
  val No = y.No
  object y {
    case object No extends YesNoDoNotKnow
  }
  val DoNotKnow = z.DoNotKnow
  object z {
    case object DoNotKnow extends YesNoDoNotKnow
  }

  def apply(optString: Option[String]): YesNoDoNotKnow = {
    val u: String = YesNoUnknown.Unknown.entryName
    val n: String = YesNoUnknown.No.entryName
    optString match {
      case Some(`u`) => YesNoDoNotKnow.DoNotKnow
      case Some(`n`) => YesNoDoNotKnow.No
      case Some(msg) => YesNoDoNotKnow.Yes(msg)
    }
  }

  def unapply(yesNoDoNotKnow: YesNoDoNotKnow): Option[String] =
    yesNoDoNotKnow match {
      case DoNotKnow => Some(YesNoUnknown.Unknown.toString)
      case No => Some(YesNoUnknown.No.toString)
      case Yes(msg) => Some(msg)
    }
}


@Singleton
class JourneyController @Inject()(
  mcc: MessagesControllerComponents,
  auditConnector: AuditConnector,
  shortLivedStore: ShortLivedStore,
  authorisedAction: AuthorisedAction,
  journeyStateStore: JourneyStateStore,
  val authConnector: AuthConnector
)(
  implicit val appConfig: AppConfig, ec: ExecutionContext
) extends FrontendController(mcc)
    with PlayInterpreter
    with I18nSupport
    with AuthorisedFunctions {

  def getState(implicit request: AuthorisedRequest[AnyContent]): Future[JourneyState] =
    journeyStateStore.getState(request.internalId)

  def setState(in: JourneyState)(implicit request: AuthorisedRequest[AnyContent]): Future[Unit] =
    journeyStateStore.storeState(request.internalId, in)

  /**
    * This clears both the journeyState and the shortLivedStore AKA save4later
    * @param request
    * @return
    */
  def clearState(implicit request: AuthorisedRequest[AnyContent]): Future[Unit] = {
    shortLivedStore.clearPersistence(request.internalId)
    journeyStateStore.clear(request.internalId)
  }

  def username(implicit  request: AuthorisedRequest[AnyContent]): String = {
    s"${request.name.name.getOrElse("")} ${request.name.lastName.getOrElse("")}"
  }


  def messages(request: Request[AnyContent]): UniformMessages[Html] =
    convertMessages(messagesApi.preferred(request)) |+| UniformMessages.bestGuess.map(HtmlFormat.escape)
  
//  def timeOut: Action[AnyContent] = Action { implicit request =>
//    implicit val msg: UniformMessages[Html] = messages(request)
//    Ok(uk.gov.hmrc.disguisedremunerationfrontend.views.html.time_out()).withNewSession
//  }

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
    views.html.main_template(title = s"${messagesIn(key.mkString("-")+".heading")} - ${messagesIn("common.title")}")(content)
  }

  override lazy val parse = super[FrontendController].parse

  def index: Action[AnyContent] = authorisedAction.async { implicit request =>
    getState.map { state =>
      implicit val msg: UniformMessages[Html] = messages(request)
      Ok(views.html.main_template(title = s"${msg("common.title")}")(views.html.index(state)))
    }
  }


  // tell uniform to not use the nested fields with radio buttons
  // for an Option[String], instead treat an empty string as None
  val optStringParser = new DataParser[Option[String]] {

    import cats.implicits._

    def bind(in: Input): Either[ErrorTree, Option[String]] = {
      in.value match {
        case Nil => Tree("required.test").asLeft
        case empty :: Nil if empty.trim == "" => none[String].asRight
        case s :: Nil => Some(s).asRight
        case _ => Tree("badValue").asLeft
      }
    }

    def unbind(a: Option[String]): Input = Tree(List(a.getOrElse("")))
  }

  // tell uniform to use the same renderer for an Option[String] as
  // is used for a String field
  val optStringHtml = new HtmlField[Option[String]] {
    import uk.gov.hmrc.disguisedremunerationfrontend.controllers.AssetsFrontend.stringHtml
    def render(
      key: String,
      values: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ) = {
      implicitly[HtmlField[String]].render(key, values, errors, messages)
    }
  }

  def contactDetails(implicit key: String): Action[AnyContent] =
    authorisedAction.async { implicit request =>

      implicit val keys: List[String] = key.split("/").toList
      import ContactDetails._

      // tell uniform to not use the nested fields with radio buttons
      // for an Option[String], instead treat an empty string as None
      implicit val optStringParserImplicit = optStringParser

      // tell uniform to use the same renderer for an Option[String] as
      // is used for a String field
      implicit val optStringHtmlImplicit = optStringHtml

      getState.flatMap { state =>
        runWeb(
          program = ContactDetails.program[FxAppend[Stack, PlayStack]](state.contactDetails)
            .useForm(automatic[Unit, Address])
            .useForm(automatic[Unit, TelAndEmail]),
          shortLivedStore.persistence(request.internalId)
        ) { data =>
          setState(state.copy(contactDetails = Some(data))) map { _ =>
            Logger.debug("completed contact details")
            Redirect(routes.JourneyController.index())
          }
        }
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

      implicit val keys: List[String] = key.split("/").toList
      import AssetsFrontend.optionHtml
      import Scheme._

      getState.flatMap { state =>
        val default: Option[Scheme] = schemeIndex.map(state.schemes(_))
        runWeb(
          program = Scheme.program[FxAppend[Stack, PlayStack]](default)
            .useForm(automatic[Unit, String])
            .useFormMap{
              case List("scheme-recipient") => {
                implicit val rev: HtmlField[Option[String]] = optionReversed[String]
                automatic[Unit, Option[String]](implicitly, implicitly, implicitly)
              }
              case _ => automatic[Unit, Option[String]]
            }
            .useForm(automatic[Unit, Option[Employer]]({
              implicit val dp: DataParser[Option[String]] = optStringParser
              implicitly[DataParser[Option[Employer]]]
            }, {
              implicit val hf: HtmlField[Option[String]] = optStringHtml
              implicitly[HtmlForm[Option[Employer]]]
            }, implicitly))
            .useForm(automatic[Unit, TaxSettlement])
            .useForm(automatic[Unit, YesNoDoNotKnow])
            .useForm(automatic[Unit, Boolean])
            .useForm(automatic[Unit, Date])
            .useForm(automatic[Unit, (Date, Date)]),
          shortLivedStore.persistence(request.internalId)
        ) { data =>
          setState(schemeIndex match {
            case Some(i) =>
              val updatedScheme = data.copy(loanDetailsProvided = default.fold(Map.empty[Year, LoanDetails])(_.loanDetailsProvided))
              state.copy(schemes = state.schemes.patch(i, Seq(updatedScheme), 1))
            case None =>
              state.copy(schemes = data :: state.schemes)
          }).map { _ =>
            Logger.debug("completed scheme")
            Redirect(routes.JourneyController.index())
          }
        }
      }
  }

  def loanDetails(
    schemeIndex: Int,
    year: Year,
    key: String
  ) = authorisedAction.async { implicit request =>
    implicit val keys: List[String] = key.split("/").toList
    import LoanDetails._
    import uk.gov.hmrc.disguisedremunerationfrontend.controllers.AssetsFrontend.stringHtml

    getState.flatMap { state =>
      val scheme = state.schemes(schemeIndex)
      val existing = scheme.loanDetails(year)
      runWeb(
        program = LoanDetails.program[FxAppend[Stack, PlayStack]](year, scheme, existing)
          .useForm(automatic[Unit, YesNoUnknown])
          .useForm(automatic[Unit, Boolean])
          .useForm(automatic[Unit, Money])
          .useForm(automatic[Unit, Option[Money]])
          .useForm(automatic[Unit, WrittenOff]),
        shortLivedStore.persistence(request.internalId)
      ){data =>
        val updatedScheme = scheme.copy(
          loanDetailsProvided = scheme.loanDetailsProvided + (year -> data))
        setState(state.copy(
          schemes = state.schemes.patch(schemeIndex, Seq(updatedScheme), 1)
        )).map { _ =>
          Logger.debug("completed loan details")
          Redirect(routes.JourneyController.index())
        }
      }
    }
  }

  implicit def renderTell: (Unit, String) => Html = {case _ => Html("")}

  val eitherParser = new DataParser[Either[Nino, Utr]] {

    import cats.implicits._


    def bind(in: Input): Either[ErrorTree, Either[Nino, Utr]] = {

      (in.value, in.children.head._2.children.values.head.value, in.children.takeRight(1).values.head.children.values.head.value) match {
        case (Nil, _, _ ) => Tree("requiredradio").asLeft
        case ("Left"::Nil, a, _) => Left(a.head).asRight
        case ("Right"::Nil, _, b) => Right(b.head).asRight

      }
    }

    def unbind(a: Either[Nino, Utr]): Input = Tree(List(a.getOrElse(""))) // TODO
  }

    def aboutYou(implicit key: String): Action[AnyContent] = authorisedAction.async {
    implicit request =>
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
              Seq("FALSE", "TRUE"),
              values.value.headOption,
              errors,
              messages
            )
        }
        automatic[Unit, Boolean]
      }
      import AboutYou._
      getState.flatMap { state =>
        runWeb(
          program = AboutYou.program[FxAppend[Stack, PlayStack]](state.aboutYou, request.nino, request.utr)
            .useFormMap {
              case List("aboutyou-completedby") => customBool
              case _ => automatic[Unit, Boolean]
            }
            .useForm(automatic[Unit, Either[Nino, Utr]]({
              implicit val a = eitherParser
              implicitly[DataParser[Either[Nino, Utr]]]
            }, implicitly, implicitly))
            .useForm(automatic[Unit, EmploymentStatus])
            .useForm(automatic[Unit, String](implicitly, {
              import uk.gov.hmrc.disguisedremunerationfrontend.controllers.AssetsFrontend.stringHtml
              implicitly[HtmlForm[String]]
            }, implicitly))
            .useForm(automatic[Unit, Unit]),
          shortLivedStore.persistence(request.internalId)
        ) {
          _ match {
            case Left(err) => {
              clearState
              Future.successful(Redirect(routes.AuthenticationController.signOut()))
            }
            case Right(data: AboutYou) =>
                setState(state.copy(aboutYou = Some(data))) map { _ =>
                  Logger.debug("completed about you for someone else without enrolments")
                  Redirect(routes.JourneyController.index())
            }
          }
        }
      }
    }

  val confirmationForm = Form(single(
    "confirm" -> boolean.verifying("error.cya.confirmation-needed", identity(_))
  ))

  def blocksFromState(
    state: JourneyState
  )(
    implicit request: AuthorisedRequest[AnyContent]
  ): List[(Html, (List[(Html, Html)], Option[(Html, Map[String, List[String]])]))] = {
    def msg(in: String): Html = messages(request)(in)
    import HtmlFormat.escape
    state match {
      case JourneyState(Some(aboutYou), schemes, Some(contactDetails)) =>
        val h: (Html, (List[(Html, Html)], Option[(Html, Map[String, List[String]])])) =
          (msg("personal-details"),(List(
            msg("name") ->
              escape(username),
            msg("filling-in-form-for-self") ->
              msg(if(aboutYou.completedBySelf) "TRUE" else "FALSE"),
            msg("address") ->
              contactDetails.address.lines.
                map(escape).
                intercalate(Html("<br />")),
            msg("contact-details") -> {
              import contactDetails.telephoneAndEmail._
              List(
                telephone.map{x => ("telephone", x)},
                email.map{x => ("Email address", x)}
              ).flatten.map{ case (l,r) =>
                msg(l) |+| Html(": ") |+| escape(r)
              }.intercalate(Html("<br />"))
            }
          ), Option.empty ))

        val t: List[(Html, (List[(Html, Html)], Option[(Html, Map[String, List[String]])]))] = schemes.map { scheme =>
          import scheme._
          val dateFormat = DateTimeFormatter.ofPattern("d MMMM YYYY")
          ((msg("scheme") |+| escape(s" $name")), (List(
            msg("dates-you-received-loans") ->
              Html(s"${scheme.schemeStart.format(dateFormat)} to ${scheme.schemeStopped.getOrElse(LocalDate.now).format(dateFormat)}"),
            msg("disclosure-of-tax-avoidance-schemes-dotas-number") ->
              dotasReferenceNumber.fold(msg("not-applicable"))(m => msg(s"disclosure-of-tax-avoidance-schemes-dotas-number.$m")),
            msg("hmrc-case-reference-number") ->
              caseReferenceNumber.fold(msg("FALSE")){escape},
            msg("employment-status") ->
              employee
                .fold(msg("FALSE")){_ => msg("Employed")},
            msg("loan-recipient") ->
              msg(if(loanRecipient) "TRUE" else "FALSE"),
            msg("tax-or-national-insurance-paid-or-agreed-to-pay") ->
              settlement.fold(msg("none")){x => Html(s"£${x.amount}")}
          ), Some(Html(s"Loan details for $name") ->
              scheme.loanDetails.flatMap(
                {
                  case(k,v) => Map(k.toString -> v.fold(List.empty[String])(ld => ld.toListString))
                }
              )
            )
          ))
        }
        h :: t
      case _ => Nil
    }
  }

  def cya: Action[AnyContent] = authorisedAction.async { implicit request =>
    implicit val m: UniformMessages[Html] = messages(request)
    getState.map { state =>
      if (!state.readyToSubmit) {
        Ok(views.html.main_template(title = s"${m("common.title")}")(views.html.index(state)))
      } else {
        val contents = views.html.cya(
          username,
          blocksFromState(state),
          confirmationForm
        )
        Logger.debug("completed check your answers")
        Ok(views.html.main_template(title =
          s"${m("cya.title")} - ${m("common.title")}")(contents)
        )
      }
    }
  }

  def cyaPost: Action[AnyContent] = authorisedAction.async { implicit request =>
    implicit val m: UniformMessages[Html] = messages(request)
    getState.map { state =>
      confirmationForm.bindFromRequest.fold(
        formWithErrors => {
          val contents =
            views.html.cya(username, blocksFromState(state), formWithErrors)
          BadRequest(views.html.main_template(
            title = s"${m("cya.title")} - ${m("common.title")}"
          )(contents))
        },
        postedForm => {

          makeAudit(java.util.UUID.randomUUID().toString, username, state)
          clearState
          Logger.info(s"submission details sent to splunk")
          val contents = views.html.confirmation(getDateTime())
          Ok(views.html.main_template(
            title = s"${m("confirm.title")} - ${m("common.title")}")
          (contents))
        }
      )
    }
  }

  case class AuditWrapper(
    submitterName: String,
    data: JourneyState
  )
  case object AuditWrapper {
    implicit val auditWrapperFormatter: OFormat[AuditWrapper] = Json.format[AuditWrapper]
  }

  def makeAudit(id: String, username: String, state: JourneyState)(implicit request: AuthorisedRequest[AnyContent]) = {
    // the audit for TXM
    auditConnector.sendExplicitAudit("disguisedRemunerationCheck", Json.toJson(AuditWrapper(username, state)))
    // the audit for RIS
    for {
      scheme <- state.schemes
      loanDetails <- scheme.loanDetailsProvided.toSeq.sortBy(_._1)
    } yield {

      val flatState = Json.toJson(FlatState(
            id,
            username,
            state.aboutYou,
            scheme.name,
            scheme.dotasReferenceNumber,
            scheme.caseReferenceNumber,
            scheme.schemeStart,
            scheme.schemeStopped,
            scheme.employee,
            scheme.loanRecipient,
            scheme.loanRecipientName,
            scheme.settlement,
            loanDetails._2.year,
            loanDetails._2.hmrcApproved,
            loanDetails._2.amount,
            loanDetails._2.genuinelyRepaid,
            loanDetails._2.writtenOff,
            state.contactDetails
      ))
      auditConnector.sendExplicitAudit(
        "disguisedRemunerationRis",
        flatState
      )
    }
  }
}
