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

package uk.gov.hmrc.disguisedremunerationfrontend.config

import javax.inject.{Inject, Singleton}
import play.api.{Configuration, Environment}
import play.api.Mode
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.duration.Duration

@Singleton
class AppConfig @Inject()(val runModeConfiguration: Configuration, servicesConfig: ServicesConfig, environment: Environment) {
  protected def mode: Mode = environment.mode

  private def loadConfig(key: String) = runModeConfiguration.getString(key).getOrElse(throw new Exception(s"Missing configuration key: $key"))

  private val contactHost = runModeConfiguration.getString(s"contact-frontend.host").getOrElse("")
  private val contactFormServiceIdentifier = loadConfig("appName")

  lazy val assetsPrefix = loadConfig(s"assets.url") + loadConfig(s"assets.version")
  lazy val analyticsToken = loadConfig(s"google-analytics.token")
  lazy val analyticsHost = loadConfig(s"google-analytics.host")
  lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

//  val mongoSessionExpireAfter: Duration = servicesConfig.getDuration("mongodb.session.expireAfter")
  val mongoShortLivedStoreExpireAfter: Duration = servicesConfig.getDuration("mongodb.shortLivedCache.expireAfter")
  val mongoJourneyStoreExpireAfter: Duration = servicesConfig.getDuration("mongodb.journeyStore.expireAfter")

  //Auth related config
  lazy val appName: String = loadConfig("appName")
  private lazy val companyAuthFrontend = servicesConfig.getConfString("company-auth.url", "")
  private lazy val companyAuthSignInPath = servicesConfig.getConfString("company-auth.sign-in-path", "")
  private lazy val companyAuthSignOutPath = servicesConfig.getConfString("company-auth.sign-out-path", "")
  lazy val ggLoginUrl: String = s"$companyAuthFrontend$companyAuthSignInPath"

  lazy val feedbackSurveyUrl: String = loadConfig("microservice.services.feedback-survey.url")
  lazy val signOutDnumUrl: String = s"$companyAuthFrontend$companyAuthSignOutPath?continue=$feedbackSurveyUrl"
  lazy val betaFeedbackUrlAuth = s"$contactHost/contact/beta-feedback?service=$contactFormServiceIdentifier"
  lazy val betaFeedbackUrlNoAuth = s"$contactHost/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier"

  lazy val dnumIndexPage: String = loadConfig("dnum-index-page-url")

  lazy val languageTranslationEnabled: Boolean =
    runModeConfiguration.get[Boolean]("microservice.services.features.welsh-translation")
}
