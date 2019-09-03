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

///*
// * Copyright 2019 HM Revenue & Customs
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
package uk.gov.hmrc.disguisedremunerationfrontend.repo

import cats.data.OptionT
import cats.implicits._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Logger
import play.api.libs.json._
import play.modules.reactivemongo.ReactiveMongoComponent
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
    val x: OptionT[Future, JourneyState] = for {
      a <- OptionT(cacheRepository.findById(userId))
      b <- OptionT.fromOption[Future](a.data)
    } yield {
      Try((b \ cacheRepositoryKey).as[JourneyState]) match {
        case Success(state) => state
        case Failure(NonFatal(ex)) => {
          Logger.info(s"Problem reading JourneyState from db, ${ex.getMessage}")
          JourneyState()
        }
      }
    }
    x.getOrElse{JourneyState()}
  }

  override def storeState(userId: String, journeyState: JourneyState): Future[Unit] =
    cacheRepository.createOrUpdate(userId, cacheRepositoryKey, Json.toJson(journeyState)).map(_=>(()))

  override def clear(userId: String): Future[Unit] =
    cacheRepository.removeById(userId).map(_=>(()))

}
