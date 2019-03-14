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

//import cats.implicits._
import javax.inject.Inject
import ltbs.uniform.ErrorTree
import ltbs.uniform.interpreters.playframework.PlayInterpreter
import ltbs.uniform.web.{Htmlable, Messages}
import play.api.i18n.I18nSupport
import play.api.libs.json.{Json} //, Reads, Writes}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import play.twirl.api.Html
//import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import uk.gov.hmrc.disguisedremunerationfrontend.data._
//import uk.gov.hmrc.disguisedremunerationfrontend.views
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

class SplunkController @Inject()(mcc: MessagesControllerComponents, auditConnector: AuditConnector)(implicit executionContext: ExecutionContext)
  extends FrontendController(mcc) with PlayInterpreter with I18nSupport {

  def acceptAndSend = Action { implicit request =>
  {
    val auditSource = "disguised-remuneration"

    // Test data
    val _address = Address(
      line1 = "11 The Hight Street",
      line2 = "Hove",
      town = "Brighton",
      county = "Sussex",
      postcode = "BN1 1AB")

    val _contactDetails = ContactDetails(
      address = _address,
      telephoneAndEmail = TelAndEmail(Some("0133 656560"), Some("dr@gov.uk"))
    )

    val _aboutYou = AboutYou(
      completedBySelf = true,
      alive = false,
      identification = Some(Left("AB123456D")),
      deceasedBefore = Some(true),
      employmentStatus = None,
      actingFor = Some("Derek")
    )

    val _scheme = Scheme(
      name = "dp02",
      dotasReferenceNumber = Some("Dotas_001"),
      caseReferenceNumber = Some("CSS-002"),
      schemeStart = Some(LocalDate.now()),
      schemeStopped = None,
      employee = Some(Employer(name = "Tax dodgers ltd", paye = "123/AB456")),
      loanRecipient = true,
      loanRecipientName = Some("Tax Dodger"),
      settlement = Some(TaxSettlement(amount = 100, dateOfSettlement = LocalDate.now()))
    )

    val _loanDetails = LoanDetails(
      scheme = _scheme,
      year = 2010,
      hmrcApproved = false,
      genuinelyRepaid = 100,
      amount = 300,
      writtenOff = Some(WrittenOff(
        amount = 300,
        taxPaid = 120
      ))
    )

    val _journeyState = JourneyState(
      aboutYou = Some(Some(_aboutYou)),
      schemes = List(_scheme),
      contactDetails = Some(_contactDetails),
      details = List(_loanDetails)
    )

    auditConnector.sendExplicitAudit(auditSource, Json.toJson(_journeyState))
    Ok(s"auditSource:${Json.toJson(_journeyState)}  -> Splunk")
  }
  }

  override def messages( request: Request[AnyContent] ): Messages = convertMessages(messagesApi.preferred(request))

  override def renderForm( key: List[String], errors: ErrorTree, form: Html, breadcrumbs: List[String], request: Request[AnyContent], messages: Messages ): Html = Html("")

  override def listingPage[A]( key: List[String], errors: ErrorTree, elements: List[A], messages: Messages )( implicit evidence$1: Htmlable[A] ): Html = Html("")
}