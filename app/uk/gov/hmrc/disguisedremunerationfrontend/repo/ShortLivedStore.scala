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
import com.google.inject.{ImplementedBy, Inject}
import javax.inject.Singleton
import ltbs.uniform.{DB, Encoded}
import ltbs.uniform.interpreters.playframework.Persistence
import play.api.libs.json._
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig

import scala.concurrent.{ExecutionContext, Future}
import cats.implicits._

@ImplementedBy(classOf[ShortLivedStoreImpl])
trait ShortLivedStore {

  def persistence(id: String): Persistence

}

@Singleton
class ShortLivedStoreImpl @Inject() (mongo: ReactiveMongoComponent)(implicit appConfig: AppConfig, ec: ExecutionContext) extends ShortLivedStore {

  private val expireAfterSeconds = appConfig.mongoShortLivedCacheExpireAfter.toSeconds

  private val cacheRepository = new CacheMongoRepository("shortLivedCache", expireAfterSeconds)(mongo.mongoConnector.db, ec)

  private val cacheRepositoryKey = "save4later"

  def persistence(userId: String): Persistence = new Persistence {

    implicit val dbFormatter: Format[DB] = new Format[DB] {
      val mapFormatter: Format[Map[String, String]] = implicitly[Format[Map[String,String]]]
      override def writes(o: DB): JsValue =
        mapFormatter.writes(o.map { case (k,v) => (k.mkString("/"), v)})
      override def reads(json: JsValue): JsResult[DB] =
        mapFormatter.reads(json).map{ _.map { case (k,v) => (k.split("/").toList, v)} }
    }

    // TODO - remove the foo, look at implicitly[Monoid[DB]].empty
    override def dataGet: Future[DB] = {
      val x: OptionT[Future, DB] = for {
        a <- OptionT(cacheRepository.findById(userId))
        b <- OptionT.fromOption[Future](a.data)
      } yield (b \ cacheRepositoryKey).as[DB]

      x.getOrElse(Map.empty)
    }

    override def dataPut(dataIn: DB): Future[Unit] =
      cacheRepository.createOrUpdate(userId, cacheRepositoryKey, Json.toJson(dataIn)).map(_ => (()))

  }

}

