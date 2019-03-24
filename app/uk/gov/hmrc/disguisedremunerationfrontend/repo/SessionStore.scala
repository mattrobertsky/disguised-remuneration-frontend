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

package uk.gov.hmrc.disguisedremunerationfrontend.repo

import cats.data.OptionT
import cats.implicits._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Format, Json}
import play.api.mvc.{AnyContent, Request}
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.model.Cache
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.DatabaseUpdate
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SessionStoreImpl])
trait SessionStore {

  def sessionUuid(implicit request: Request[AnyContent]): Option[String] =  request.session.get("uuid")

  def get[A](sessionId: String, key: String)(implicit hc: HeaderCarrier, ec: ExecutionContext, format: Format[A]): OptionT[Future, A]

  def store[A](sessionId: String, key: String, data: A)
              (implicit hc: HeaderCarrier, ec: ExecutionContext, format: Format[A]): Future[DatabaseUpdate[Cache]]
}

@Singleton
class SessionStoreImpl @Inject() (mongo: ReactiveMongoComponent)(implicit appConfig: AppConfig, ec: ExecutionContext) extends SessionStore {

  private val expireAfterSeconds = appConfig.mongoSessionExpireAfter.toSeconds

  private val cacheRepository = new CacheMongoRepository("sessions", expireAfterSeconds)(mongo.mongoConnector.db, ec)

  private type EitherStringOr[A] = Either[String, A]

  override def get[A]( sessionId: String, key: String )( implicit hc: HeaderCarrier, ec: ExecutionContext, format: Format[A] ): OptionT[Future, A] = {
    for {
      a <- OptionT(cacheRepository.findById(sessionId))
      b <- OptionT.fromOption[Future](a.data)
    } yield ( b \ key).as[A]
  }

  override def store[A]( sessionId: String, key: String, data: A)(implicit hc: HeaderCarrier, ec: ExecutionContext, format: Format[A] ): Future[DatabaseUpdate[Cache]] = {
    cacheRepository.createOrUpdate(sessionId, key, Json.toJson(data))
  }
}