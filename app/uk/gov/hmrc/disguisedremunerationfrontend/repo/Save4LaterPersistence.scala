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
import com.google.inject.{ImplementedBy, Inject}
import javax.inject.Singleton
import ltbs.uniform.interpreters.playframework.{DB, PersistenceEngine}
import play.api.libs.json._
import play.api.mvc.{AnyContent, Result}
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.disguisedremunerationfrontend.actions.AuthorisedRequest
import uk.gov.hmrc.disguisedremunerationfrontend.config.AppConfig

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[AbstractPersistence])
trait Persistence {

  def persistence: PersistenceEngine[AuthorisedRequest[AnyContent]]
  def clearPersistence(implicit request: AuthorisedRequest[AnyContent]): Future[Unit]

}

abstract class AbstractPersistence extends Persistence with Store {

  implicit val executionContext: ExecutionContext

  implicit val dbFormatter: Format[DB] = new Format[DB] {
    val mapFormatter: Format[Map[String, String]] = implicitly[Format[Map[String,String]]]
    override def writes(o: DB): JsValue =
      mapFormatter.writes(o.map { case (k,v) => (k.mkString("/"), v)})
    override def reads(json: JsValue): JsResult[DB] =
      mapFormatter.reads(json).map{ _.map { case (k,v) => (k.split("/").toList, v)} }
  }

  def makeKey(path: String): String = {
    path.replaceFirst("^.*\\/disguised-remuneration\\/", "").split("/").toList match {
      case a if a.exists(_.matches("[0-9]")) => a.slice(0, a.lastIndexWhere(_.matches("[0-9]")) +1).mkString("/")
      case b if b.exists(_.matches("new")) => b.slice(0, b.lastIndexWhere(_.matches("new")) +1).mkString("/")
      case c => c.head
    }
  }

}

abstract class Save4LaterPersistence @Inject() (
  mongo: ReactiveMongoComponent
)(
  implicit appConfig: AppConfig,
  val executionContext: ExecutionContext
) extends AbstractPersistence with JourneyStore {

  val expireAfterSeconds: Long = appConfig.mongoShortLivedStoreExpireAfter.toSeconds
  val cacheRepository: CacheMongoRepository =
    new CacheMongoRepository("shortLivedCache", expireAfterSeconds)(mongo.mongoConnector.db, executionContext)
  val cacheRepositoryKey: String = "save4later"


  def clearPersistence(implicit request: AuthorisedRequest[AnyContent]): Future[Unit] =
    cacheRepository.removeById(request.internalId).map(_ => ())

  override def persistence: PersistenceEngine[AuthorisedRequest[AnyContent]] =
    new PersistenceEngine[AuthorisedRequest[AnyContent]] {

      import play.api.libs.json._

      def load(userId: String, path: String): Future[DB] = {
        val x: OptionT[Future, DB] = for {
          a <- OptionT(cacheRepository.findById(userId))
          b <- OptionT.fromOption[Future](a.data)
          c <- OptionT.fromOption[Future]{
                 if ((b \ path).isDefined) {
                    (b \ path).toOption
                 } else {
                    (b \ cacheRepositoryKey).toOption
                 }
               }
        } yield c.as[DB]

        x.getOrElse(Map.empty)
      }

      def save(userId: String, path: String, db: DB): Future[Unit] =
        cacheRepository.createOrUpdate(userId, path, Json.toJson(db)).map{_ =>(())}


      def clear(userId: String, path: String): Future[Unit] =
        save(userId, path, Map.empty)

      override def apply(request: AuthorisedRequest[AnyContent])(f: DB ⇒ Future[(DB, Result)]): Future[Result] = {

        val path = makeKey(request.path)
        val userId = request.internalId

        for {
          db              <- load(userId, path)
          (newDb, result) <- f(db)
          _               <- save(userId, path, newDb)
        } yield result

      }

    }

}

@Singleton
class AboutYouSave4LaterPersistence @Inject() (
  mongo: ReactiveMongoComponent
)(
  implicit appConfig: AppConfig,
  override val executionContext: ExecutionContext
) extends Save4LaterPersistence(mongo) with JourneyStore {

  override val journeyKey: String = "aboutYou"
}

@Singleton
class ContactDetailsSave4LaterPersistence @Inject() (
  mongo: ReactiveMongoComponent
)(
  implicit appConfig: AppConfig,
  override val executionContext: ExecutionContext
) extends Save4LaterPersistence(mongo) with JourneyStore {

  override val journeyKey: String = "contactDetails"

}

@Singleton
class SchemeSave4LaterPersistence @Inject() (
  mongo: ReactiveMongoComponent
)(
  implicit appConfig: AppConfig,
  override val executionContext: ExecutionContext
) extends Save4LaterPersistence(mongo) with JourneyStore {

  override val journeyKey: String = "schemes"

}

@Singleton
class LoanDetailsSave4LaterPersistence @Inject() (
  mongo: ReactiveMongoComponent
)(
  implicit appConfig: AppConfig,
  override val executionContext: ExecutionContext
) extends Save4LaterPersistence(mongo) with JourneyStore {

  override val journeyKey: String = "loans"

}