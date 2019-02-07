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

package controllers

import ltbs.uniform.interpreters.playframework._
import cats.kernel.Monoid
import javax.inject._
import play.api._
import play.api.mvc._

import org.atnos.eff._
import cats.implicits._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.i18n._

import ltbs.uniform.web.{Messages => _, _}, parser._
import ltbs.uniform.ErrorTree
import play.twirl.api.Html
import InferParser._
import ltbs.uniform.widgets.govuk._
import dr._

import scala.concurrent.Future
import play.api.i18n.{I18nSupport, MessagesApi}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import uk.gov.hmrc.disguisedremunerationfrontend.views


case class JourneyState (
  aboutYou: Option[Option[AboutYou]] = None, // Some(None) refers to user representing themselves
  contactDetails: Option[ContactDetails] = None,
  schemes: List[Scheme] = Nil,
  details: List[LoanDetails] = Nil
) {
  def detailsStatus: List[(Scheme, Int, Option[LoanDetails])] = ???

  def readyToSubmit = aboutYou.isDefined && contactDetails.isDefined &&
    schemes.nonEmpty && detailsStatus.forall(_._3.isDefined)
}


@Singleton
class JourneyController @Inject()(val messagesApi: MessagesApi, implicit val appConfig: AppConfig) extends FrontendController with PlayInterpreter with I18nSupport {

  var state: JourneyState = JourneyState()

  def messages(request: Request[AnyContent]): ltbs.uniform.web.Messages = convertMessages(messagesApi.preferred(request))

  def renderForm(key: String, errors: ErrorTree, form: Html, breadcrumbs: List[String], request: Request[AnyContent], messagesIn: ltbs.uniform.web.Messages): Html = {
    ???
    //views.html.chrome(key, errors, form, "/" :: breadcrumbs)(messagesIn, request)
  }

  def index = Action { request =>
    ???
    //Ok(views.html.index(state)(messages(request)))
  }

  def aboutYou(implicit key: String) = Action.async { implicit request =>
    import dr.AboutYou._

    runWeb(
      program = program[FxAppend[Stack, PlayStack]]
        .useForm(PlayForm.automatic[Boolean])        
        .useForm(PlayForm.automatic[Option[Date]])
        .useForm(PlayForm.automatic[Either[Nino,Utr]])
        .useForm(PlayForm.automatic[Address]),
      persistence
    ){data =>
      state = state.copy(aboutYou = Some(data))
      Future.successful(Redirect(".."))
    }
  }

  def contactDetails(implicit key: String) = Action.async { implicit request =>
    import dr.ContactDetails._

    runWeb(
      program = program[FxAppend[Stack, PlayStack]]
        .useForm(PlayForm.automatic[Address])
        .useForm(PlayForm.automatic[(String,String)]),
      persistence
    ){data =>
      state = state.copy(contactDetails = Some(data))
      Future.successful(Redirect(".."))
    }
  }

  def addScheme(implicit key: String) = Action.async { implicit request =>
    import dr.Scheme._

    runWeb(
      program = program[FxAppend[Stack, PlayStack]]
        .useForm(PlayForm.automatic[String])
        .useForm(PlayForm.automatic[Option[String]])
        .useForm(PlayForm.automatic[Date])
        .useForm(PlayForm.automatic[Option[Date]])
        .useForm(PlayForm.automatic[Boolean])
        .useForm(PlayForm.automatic[Option[TaxSettlement]]),
      persistence
    ){data =>
      state = state.copy(schemes = data :: state.schemes)
      Future.successful(Ok(data.toString))
    }
  }

  def loanDetails(year: Int, schemeNo: Int)(implicit key: String) = Action.async { implicit request =>
    import dr.LoanDetails._

    def scheme: Scheme = state.schemes(schemeNo)

    runWeb(
      program = program[FxAppend[Stack, PlayStack]](scheme, year)
        .useForm(PlayForm.automatic[Money])
        .useForm(PlayForm.automatic[Boolean])
        .useForm(PlayForm.automatic[Option[WrittenOff]]),
      persistence
    ){data =>
      state = state.copy(details = data :: state.details)
      Future.successful(Redirect(".."))
    }
  }
  
  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }

}
