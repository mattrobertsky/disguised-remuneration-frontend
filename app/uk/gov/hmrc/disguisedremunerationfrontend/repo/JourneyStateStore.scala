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

import cats.data.{EitherT, OptionT}
import cats.implicits._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Logger
import play.api.libs.json._
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.model.Cache
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig
import uk.gov.hmrc.disguisedremunerationfrontend.data.JourneyState
import uk.gov.hmrc.disguisedremunerationfrontend.data.JsonConversion._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

@ImplementedBy(classOf[JourneyStateStoreImpl])
trait JourneyStateStore {

  def getState(userId: String): Future[JourneyState]
  def storeState(userId: String, journeyState: JourneyState): Future[Unit]
  def clear(userId: String): Future[Unit]

}

@Singleton
class JourneyStateStoreImpl @Inject() (
  mongo: ReactiveMongoComponent
)(implicit appConfig: AppConfig, ec: ExecutionContext) extends JourneyStateStore with Store {

  override val expireAfterSeconds: Long = appConfig.mongoJourneyStoreExpireAfter.toSeconds
  override val cacheRepository: CacheMongoRepository =
    new CacheMongoRepository("journeyStateStore", expireAfterSeconds)(mongo.mongoConnector.db, ec)
  override val cacheRepositoryKey: String = "journeyState"

  override def getState(userId: String): Future[JourneyState] = {
    (for {
      cache <- EitherT.fromOptionF(cacheRepository.findById(userId), JourneyState())
      state <- EitherT.fromEither[Future](readCacheData(cache.data.getOrElse(JsNull)))
    } yield state).merge
  }

  override def storeState(userId: String, journeyState: JourneyState): Future[Unit] =
    cacheRepository.createOrUpdate(userId, cacheRepositoryKey, Json.toJson(journeyState)).map(_=>(()))

  override def clear(userId: String): Future[Unit] =
    cacheRepository.removeById(userId).map(_=>(()))

  /*
     Returns Either the Cache[ed] JourneyState (Right) or an empty JourneyState (Left).
     Logs info on the underlying JsResultException for the Left.
  */
  private def readCacheData(jsValue: JsValue):Either[JourneyState, JourneyState] =
    Try((jsValue \ cacheRepositoryKey).as[JourneyState]) match {
      case Success(state) =>
        Right(state)
      case Failure(NonFatal(exception)) => {
        Logger.info(s"Problem reading JourneyState from cache, ${exception.getMessage}")
        Left(JourneyState())
      }
    }

}
