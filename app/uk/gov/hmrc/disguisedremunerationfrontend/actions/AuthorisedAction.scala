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

package uk.gov.hmrc.disguisedremunerationfrontend.actions

import play.api.mvc._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import javax.inject.Inject

import play.api.Logger
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.auth.core.AuthProvider.{GovernmentGateway, Verify}
import uk.gov.hmrc.auth.core.retrieve.Retrievals._
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.auth.core.retrieve.Name
import uk.gov.hmrc.disguisedremunerationfrontend.data.{Utr, Nino}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

class AuthorisedAction @Inject()(mcc: MessagesControllerComponents, val authConnector: AuthConnector)
                                         (implicit val appConfig: AppConfig, val executionContext: ExecutionContext)
  extends ActionBuilder[AuthorisedRequest, AnyContent] with ActionRefiner[Request, AuthorisedRequest] with AuthorisedFunctions {

  override protected def refine[A](request: Request[A]): Future[Either[Result, AuthorisedRequest[A]]] = {
    implicit val req: Request[A] = request
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    val retrieval = internalId and nino and saUtr and name

      authorised(AuthProviders(GovernmentGateway, Verify)).retrieve(retrieval) { case id ~ natInsNo ~ sUtr ~ name =>
        val internalId = id.getOrElse(throw new RuntimeException("No internal ID for user"))
        Future.successful(Right(AuthorisedRequest(internalId, natInsNo, sUtr, request, name)))
    } recover {
      case _: NoActiveSession =>
        Logger.info(s"Recover - no active session")
        Left(
          Redirect(uk.gov.hmrc.disguisedremunerationfrontend.controllers.routes.AuthenticationController.signIn()))
    }
  }

  override def parser = mcc.parsers.anyContent
}

case class AuthorisedRequest[A](internalId: String, nino: Option[Nino], utr: Option[Utr], request: Request[A], name: Name)
    extends WrappedRequest(request)
