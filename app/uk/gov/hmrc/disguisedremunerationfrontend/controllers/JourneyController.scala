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
package controllers

import java.time.LocalDate

import actions.{AuthorisedAction, AuthorisedRequest}
import config.AppConfig
import data._, JsonConversion._
import repo._
import uk.gov.hmrc.disguisedremunerationfrontend.views

import cats.implicits._
import javax.inject.{Inject, Singleton}
import ltbs.uniform.common.web.GenericWebTell
import ltbs.uniform.interpreters.playframework.tellTwirlUnit
import ltbs.uniform.{ErrorTree, UniformMessages}
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc._
import play.twirl.api.Html
import play.twirl.api.HtmlFormat.escape
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendHeaderCarrierProvider

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

@Singleton
class JourneyController @Inject()(
  mcc: MessagesControllerComponents,
  auditConnector: AuditConnector,
  aboutYouSave4Later: AboutYouSave4LaterPersistence,
  contactDetailsSave4Later: ContactDetailsSave4LaterPersistence,
  schemeSave4Later: SchemeSave4LaterPersistence,
  loanDetailsSave4Later: LoanDetailsSave4LaterPersistence,
  authorisedAction: AuthorisedAction,
  journeyStateStore: JourneyStateStore,
  val authConnector: AuthConnector
)(
  implicit val appConfig: AppConfig,
  ec: ExecutionContext,
  implicit val messagesApi: MessagesApi
) extends ControllerHelpers
  with FrontendHeaderCarrierProvider
  with I18nSupport
  with AuthorisedFunctions {

  lazy val interpreter = DRInterpreter(appConfig, this, messagesApi)
  import interpreter._

  def getState(implicit request: AuthorisedRequest[AnyContent]): Future[JourneyState] =
    journeyStateStore.getState(request.internalId)

  def setState(in: JourneyState)(implicit request: AuthorisedRequest[AnyContent]): Future[Unit] =
    journeyStateStore.storeState(request.internalId, in)

  /**
    * Clears the journeyState and the save4later
    *
    * @param request
    * @return
    */
  def clearState(implicit request: AuthorisedRequest[AnyContent]): Future[Unit] = {
    journeyStateStore.clear(request.internalId)
    aboutYouSave4Later.clearPersistence
    contactDetailsSave4Later.clearPersistence
    schemeSave4Later.clearPersistence
    loanDetailsSave4Later.clearPersistence
  }

  def username(implicit request: AuthorisedRequest[AnyContent]): String = {
    s"${request.name.name.getOrElse("")} ${request.name.lastName.getOrElse("")}"
  }

  //  def timeOut: Action[AnyContent] = Action { implicit request =>
  //    implicit val msg: UniformMessages[Html] = messages(request)
  //    Ok(uk.gov.hmrc.disguisedremunerationfrontend.views.html.time_out()).withNewSession
  //  }

  //  override lazy val parse = super[FrontendController].parse

  def index: Action[AnyContent] = authorisedAction.async { implicit request =>
    getState.map { state =>
      implicit val msg: UniformMessages[Html] = interpreter.messages(request)
      Ok(views.html.main_template(
        title =
          s"${msg("common.title.short")} - ${msg("common.title")}"
      )(views.html.index(state)))
    }
  }

  private def updateStateAndRedirect(
    message: String,
    state: JourneyState,
    redirect: Result = Redirect(routes.JourneyController.index())
  )(
    implicit request: AuthorisedRequest[AnyContent]
  ) =
    setState(state).map {_=>
      Logger.debug(message)
      redirect
    }

  def aboutAction(targetId: String): Action[AnyContent] = authorisedAction.async {
    implicit request: AuthorisedRequest[AnyContent] =>
      implicit val persistence = aboutYouSave4Later.persistence
      import uk.gov.hmrc.disguisedremunerationfrontend.data.AboutYou._

      implicit val playTell: GenericWebTell[AboutYou.NoNeedToComplete.type, Html] = new GenericWebTell[NoNeedToComplete.type, Html] {
        override def render(in: NoNeedToComplete.type, key: String, messages: UniformMessages[Html]): Html =
          views.html.uniform.standard_field(List(key), ErrorTree.empty, messages)(Html(""))
      }

      getState.flatMap { state =>
        aboutYouProgram[WM](
          interpreter.create[TellTypes, AskTypes](interpreter.messages(request)),
          request.nino,
          request.utr
        ).run(targetId, false) {
          case Left(_) =>
            clearState map { _ => Redirect(routes.AuthenticationController.signOut()) }
          case Right(data: AboutYou) =>
            updateStateAndRedirect(
              "completedAboutYou",
              state.copy(aboutYou = Some(data))
            )
          }
        }
      }

  def contactDetailsAction(targetId: String) = authorisedAction.async {
    import uk.gov.hmrc.disguisedremunerationfrontend.data.ContactDetails.{AskTypes, TellTypes, contactDetailsProgram}
    implicit request: AuthorisedRequest[AnyContent] =>
      implicit val persistence = contactDetailsSave4Later.persistence
      val playProgram = contactDetailsProgram[WM](
        interpreter.create[TellTypes, AskTypes](interpreter.messages(request))
      )
      getState.flatMap { state =>
        playProgram.run(targetId) {
          case data: ContactDetails =>
            updateStateAndRedirect("completed contact details", state.copy(contactDetails = Some(data)))
        }
      }
  }

  def addScheme(key: String) = schemeAction(None, key)

  def editScheme(
    schemeIndex: Int,
    key: String
  ) = schemeAction(Some(schemeIndex), key)


  def schemeAction(schemeIndex: Option[Int], targetId: String) = authorisedAction.async {
    import uk.gov.hmrc.disguisedremunerationfrontend.data.Scheme.{AskTypes, TellTypes, schemeProgram}
    implicit request: AuthorisedRequest[AnyContent] =>
      implicit val persistence = schemeSave4Later.persistence
      getState.flatMap { state =>
        val default = schemeIndex.map(state.schemes(_))
        val result = updateStateAndRedirect("completed scheme details", _: JourneyState)
        val playProgram = schemeProgram[WM](
          interpreter.create[TellTypes, AskTypes](interpreter.messages(request)),
          default
        )
        playProgram.run(targetId, true) {
          data: Scheme =>
            schemeIndex match {
              case Some(i) =>
                val updatedScheme = data.copy(loanDetailsProvided = default.fold(Map.empty[Year, LoanDetails])(_.loanDetailsProvided))
                result(state.copy(schemes = state.schemes.patch(i, Seq(updatedScheme), 1)))
              case None =>
                result(state.copy(schemes = state.schemes :+ data))
            }
        }
      }
  }

  def loanDetailsAction(schemeIndex: Int = 0, year: Int = 2019, targetId : String): Action[AnyContent] = authorisedAction.async {
    import uk.gov.hmrc.disguisedremunerationfrontend.data.LoanDetails.{AskTypes, TellTypes, loanDetailsProgram}
    implicit request: AuthorisedRequest[AnyContent] =>
      implicit val persistence = loanDetailsSave4Later.persistence
      implicit val messages = messagesApi.preferred(request)
      getState.flatMap { state =>
        val scheme = state.schemes(schemeIndex)
        val result = updateStateAndRedirect("completed loan details", _: JourneyState)
        val playProgram = loanDetailsProgram[WM](
          interpreter.create[TellTypes, AskTypes](interpreter.messages(request)),
          year,
          messages,
          scheme.name,
          scheme.loanDetails(year)
        )
        playProgram.run(targetId, true) {
          data: LoanDetails =>
            result(
              state.copy(
                schemes =
                  state.schemes.patch(
                    schemeIndex,
                    Seq(
                      scheme.copy(loanDetailsProvided = scheme.loanDetailsProvided.updated(year, data))
                    ),
                    1
                  )
              )
            )
        }
      }

  }

  val confirmationForm = Form(single(
    "confirm" -> boolean.verifying("error.cya.confirmation-needed", identity(_))
  ))

  private def msg(in: String)(implicit request: AuthorisedRequest[AnyContent]) = {
    messages(request)(in)
  }

  private def personalDetailsBlock(
    state: JourneyState,
    retrievedNino: Boolean
  )(
    implicit request: AuthorisedRequest[AnyContent]
  ): Html = {
    implicit val m: UniformMessages[Html] = messages(request)
    state match {
      case JourneyState(Some(aboutYou), _, Some(contactDetails)) =>
        val checkYourNinoOrUtr = aboutYou match {
          case a@AboutSelf(ninoOrUtr) =>
            ninoOrUtr match {
              case Left(nino) => List((msg("nino"), escape(nino), if(!retrievedNino)"about-you/your-ni-no".some else None))
              case Right(utr) => List((msg("utr"), escape(utr), None))
            }
          case a@AboutAnother(_, ninoOrUtr, _, _, _) =>
            ninoOrUtr match {
              case Left(nino) =>  List((msg("nino"), escape(nino), "about-you/about-scheme-user".some))
              case Right(utr) =>  List((msg("utr"), escape(utr), "about-you/about-scheme-user".some))
              case _ => List.empty
            }
          case _ => List.empty
        }
        views.html.answer_list(
          msg("personal-details"),
          List(
            (msg("name"), escape(username), None),
            (msg("filling-in-form-for-self"), msg(if(aboutYou.completedBySelf) "TRUE" else "FALSE"), "about-you/about-you".some),
            checkYourNinoOrUtr.headOption.getOrElse((Html(""), Html(""), None)),
            (msg("address"), contactDetails.address.lines.map(escape).intercalate(Html("<br />")), "contact-details/confirm-contact-details".some),
            (msg("contact-details"), {
              import contactDetails.telephoneAndEmail._
              List(
                telephone.some.map{x => ("telephone", x)},
                email.some.map{x => ("email-address", x)}
              ).flatten.map{ case (l,r) =>
                msg(l) |+| Html(": ") |+| escape(r)
              }.intercalate(Html("<br />"))
            }, "contact-details/confirm-contact-prefs".some)
          )
        )
    }
  }

  private def schemeBlock(
    state: JourneyState
  )(
    implicit request: AuthorisedRequest[AnyContent]
  ): Html = {
    implicit val ufMsg: UniformMessages[Html] = messages(request)
    state match {
      case JourneyState(Some(_), schemes, Some(_)) =>
        Html(schemes.zipWithIndex.map { case(scheme, index) =>
          import scheme._
          views.html.answer_list(
            msg("scheme") |+| escape(s" $name"),
            List(
              (msg("dates-you-received-loans"),
                Html(s"${formatDate(scheme.schemeStart)} ${msg("language.to")} ${formatDate(scheme.schemeStopped.getOrElse(LocalDate.now))}"),
                scheme.schemeStopped.fold(s"scheme/$index/still-using-the-scheme-yes")(_=> s"scheme/$index/still-using-the-scheme-no").some),
              (msg("disclosure-of-tax-avoidance-schemes-dotas-number"),
                dotasReferenceNumber.fold(msg("not-applicable"))(m => msg(s"disclosure-of-tax-avoidance-schemes-dotas-number.$m")),
                s"scheme/$index/dotas-number".some),
              (msg("hmrc-case-reference-number"),
                caseReferenceNumber.fold(msg("FALSE")){escape},
                s"scheme/$index/case-reference-number".some),
              (msg("employment-status"),
                employee.fold(msg("FALSE")){_ => msg("employed")},
                s"scheme/$index/user-employed".some),
              (msg("loan-recipient"),
                msg(if(loanRecipient) "TRUE" else "FALSE"),
                s"scheme/$index/about-loan".some),
              (msg("tax-or-national-insurance-paid-or-agreed-to-pay"),
                settlement.fold(
                  msg(s"tax-or-national-insurance-paid-or-agreed-to-pay.$settlementAgreed")
                ){taxSettlement => Html(s"£${taxSettlement.amount}")},
                settlement.fold(s"scheme/$index/tax-settled")(_ => s"scheme/$index/add-settlement").some )
            )
          ) |+|
            Html(
              scheme.loanDetails.map(
                {
                  case(k,v) => {
                    val isHmrcApproved = v.fold(Html(""), Html(""), None: Option[String])( ld =>
                          ld.hmrcApproved.fold(Html(""), Html(""), None: Option[String])(ynu =>
                            (msg(s"cya.loandetails.header.fixed-amount"),
                            msg(s"fixed-term-loan.$ynu"),
                            s"scheme/$index/details/$k/fixed-term-loan".some)
                          )
                    )

                    views.html.answer_list(
                      msg("cya.loandetails.h1") |+| escape(s" $name"),
                      List(
                        (msg("cya.loandetails.header.tax-year"),
                          Html(s"${k.toString} ${msg("language.to")} ${(k.toInt + 1).toString}"),
                          None),
                        isHmrcApproved,
                        (msg("cya.loandetails.header.amount"),
                          v.fold(Html("£0"))( x => Html(s"£${x.totalLoan.amount}")),
                          s"scheme/$index/details/$k/loan-amount".some),
                        (Html(s"${ufMsg(
                          "cya.loandetails.header.estimate",
                          formatDate(LocalDate.of(k, 4, 6), "d MMMM yyyy"),
                          formatDate(LocalDate.of(k + 1, 4, 5), "d MMMM yyyy"))}"
                        ),
                          v.fold(Html("£0"))( x => msg(if(x.totalLoan.estimate)"TRUE" else "FALSE")),
                          s"scheme/$index/details/$k/loan-amount".some),
                        (msg("cya.loandetails.header.repaid"),
                          v.fold(Html("£0"))( x => Html(s"£${x.genuinelyRepaid.getOrElse("0")}")),
                          v.fold(s"scheme/$index/details/$k/repaid-any-loan-during-tax-year".some){
                            x =>
                              if (x.isGenuinelyRepaid) s"scheme/$index/details/$k/loan-repaid".some
                              else s"scheme/$index/details/$k/repaid-any-loan-during-tax-year".some
                          }),
                        (msg("cya.loandetails.header.written-off"),
                          v.fold(Html("£0"))( x =>  Html(s"£${x.writtenOff.fold("0")(x => x.amount.toString())}")),
                          s"scheme/$index/details/$k/written-off".some),
                        (msg("cya.loandetails.header.tax-paid"),
                          v.fold(Html("£0"))( x =>  Html(s"£${x.writtenOff.fold("0")(x => x.taxPaid.toString())}")),
                          s"scheme/$index/details/$k/written-off".some)
                      ).filterNot{x => x._1 == Html("") && x._2 == Html("") && x._3.isEmpty}
                    )
                  }
              }).mkString
            )
        }.mkString)
    }
  }

  private def blocksFromState(
    state: JourneyState,
    retrievedNino: Boolean
  )(
    implicit request: AuthorisedRequest[AnyContent]
  ): Html = {
    personalDetailsBlock(state, retrievedNino) |+| schemeBlock(state)
  }

  def cya: Action[AnyContent] = authorisedAction.async { implicit request =>
    implicit val m: UniformMessages[Html] = messages(request)
    getState.map { state =>
      if (!state.readyToSubmit) {
        Ok(views.html.main_template(title = s"${m("common.title")}")(views.html.index(state)))
      } else {
        val contents = views.html.cya(
          username,
          blocksFromState(state, request.nino.nonEmpty),
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
            views.html.cya(username, blocksFromState(state, request.nino.nonEmpty), formWithErrors)
          BadRequest(views.html.main_template(
            title = s"${m("cya.title")} - ${m("common.title")}"
          )(contents))
        },
        postedForm => {

          makeAudit(java.util.UUID.randomUUID().toString, username, state)(request)
          val contents = views.html.confirmation(
            formatDate(LocalDate.now()),
            formattedTimeNow,
            state.aboutYou.map {
              case AboutAnother(_, _, _, _, actingFor) => actingFor
              case _ => ""
            }.getOrElse("")
          )
          clearState
          Logger.info(s"submission details sent to splunk")
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

  def makeAudit(id: String, username: String, state: JourneyState)(implicit request: AuthorisedRequest[AnyContent]): List[Unit] = {
    // remove spaces from nino for splunk
    val cleanNinoState = state.aboutYou match {
      case Some(a@AboutSelf(ninoOrUtr)) =>
        ninoOrUtr match {
          case Left(spaceyNino) => state.copy(aboutYou = a.copy(ninoOrUtr = Left(spaceyNino.replace(" ", ""))).some)
          case Right(utr) => state.copy(aboutYou = a.copy(ninoOrUtr = Right(utr.replace(" ", ""))).some)
        }
      case Some(a@AboutAnother(_, Left(nino), _, _, _)) =>
        state.copy(aboutYou = a.copy(identification = Left(nino.replace(" ", ""))).some)
      case _ =>
        state
    }
    // the audit for TXM
    auditConnector.sendExplicitAudit("disguisedRemunerationCheck", Json.toJson(AuditWrapper(username, cleanNinoState)))


    // the audit for RIS
    for {
      scheme <- state.schemes
      loanDetails <- scheme.loanDetailsProvided.toSeq.sortBy(_._1)
    } yield {

      val flatState = Json.toJson(FlatState(
            id,
            username,
            cleanNinoState.aboutYou,
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
            loanDetails._2.totalLoan,
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
