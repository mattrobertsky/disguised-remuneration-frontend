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

package uk.gov.hmrc.disguisedremunerationfrontend.data


import ltbs.uniform._
import org.atnos.eff._
import play.api.libs.json.{JsNull, JsValue, Json, Writes}
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.EmploymentStatus

case class AboutYou(
  completedBySelf: Boolean,
  alive: Boolean,
  identification: Option[Either[Nino, Utr]] = None,
  deceasedBefore: Option[Boolean] = None,
  employmentStatus: Option[EmploymentStatus] = None,
  actingFor: Option[String] = None
)

case class NotRequiredToComplete(
  p1: String = "<p>Based on your answers, you do not need to send any loan charge details.</p>",
  p2: String = "<p>This is because you told us the deceased person:",
  p3: String = "<ul class=\"govuk-list govuk-list--bullet\">\n          <li>was employed when they had the loan</li>\n          <li>has died on or before 5 April 2019</li>\n        </ul>",
  p4: String = "<p>If this is not right, you need to <a class=\"govuk-link\" href=\"/disguised-remun-prototype/4-0/was-user-self-employed?user-alive=No\">go back and change your answers<a/>.</p>"
)

object AboutYou {

  implicit def eitherWrites[Nino, Utr](implicit Nino: Writes[Nino], Utr: Writes[Utr]): Writes[Either[Nino, Utr]] =
    Writes[Either[Nino, Utr]] {
      case Left(l) => Nino.writes(l)
      case Right(r) => Utr.writes(r)
    }

  implicit val aboutYouWrites = new Writes[AboutYou] {
    override def writes(o: AboutYou ): JsValue = Json.obj(
      "completedBySelf" -> o.completedBySelf,
      "alive" -> o.alive,
      "identification" -> o.identification,
      "deceasedBefore" -> o.deceasedBefore,
      "employmentStatus" -> o.employmentStatus,
      "actingFor" -> o.actingFor
    )
  }

  implicit val optAboutYouWrites =
    Writes[Option[Option[AboutYou]]] {
      case Some(o) => o
      match {
        case Some(oo) => aboutYouWrites.writes(oo)
        case _ => JsNull
      }
      case _ => JsNull
    }


  // Move into utils
  lazy val regExUTR = """^(?:[ \t]*(?:[a-zA-Z]{3})?\d[ \t]*){10}$"""
  lazy val regExNino = """^((?!(BG|GB|KN|NK|NT|TN|ZZ)|(D|F|I|Q|U|V)[A-Z]|[A-Z](D|F|I|O|Q|U|V))[A-Z]{2})[0-9]{6}[A-D]?$"""

  sealed trait Error
  case object NoNeedToComplete extends Error

  type Stack = Fx.fx5[
    UniformAsk[Boolean,?],
    UniformAsk[String,?],
    UniformAsk[Unit,?],
    UniformAsk[Either[Nino,Utr],?],
    UniformAsk[EmploymentStatus,?]
  ]

  def program[R
      : _uniformCore
      : _uniformAsk[Boolean,?]
      : _uniformAsk[String,?]
      : _uniformAsk[Either[Nino,Utr],?]
      : _uniformAsk[EmploymentStatus,?]
      : _uniformAsk[Unit,?]
  ](default: Option[Option[AboutYou]]): Eff[R, Either[Error,Option[AboutYou]]] = 
    for {
      completedBy <- ask[Boolean]("aboutyou-completedby")
                       .defaultOpt(default.map(_.isDefined))
      ret <- completedBy match {
        case false => Eff.pure[R,Either[Error,Option[AboutYou]]](Right(None))
        case true => for {
          alive   <- ask[Boolean]("aboutyou-personalive")
                       .defaultOpt(default.flatMap(_.map(_.alive)))
          employmentStatus  <- ask[EmploymentStatus]("aboutyou-employmentstatus")
                                 .defaultOpt(default.flatMap(_.flatMap(_.employmentStatus))).in[R] when !alive
          deceasedBefore  <- ask[Boolean]("aboutyou-deceasedbefore")
                                  .defaultOpt(default.flatMap(_.flatMap(_.deceasedBefore)))
                                  .in[R] when employmentStatus == Some(EmploymentStatus.Employed)
          notRequiredToComplete = deceasedBefore == Some(true)
          _ <- tell[Unit]("aboutyou-noloancharge")("_").in[R] when notRequiredToComplete
          id <- ask[Either[Nino,Utr]]("aboutyou-identity")
                  .defaultOpt(default.flatMap(_.flatMap(_.identification)))
                  .validating(
                    "Enter the person's National Insurance number in the correct format",
                    _ match {
                      case Left(nino) => nino.matches(regExNino)
                      case _ => true
                    }
                  )
                  .validating(
                    "Enter the person's Self Assessment Unique Taxpayer Reference (UTR)",
                    _ match {
                      case Left(nino) => true
                      case Right(utr) => utr.matches(regExUTR)
                    }
                  )
                  .in[R] when (!notRequiredToComplete)
          personName <- ask[String]("aboutyou-confirmation")                  
                          .defaultOpt(default.flatMap(_.flatMap(_.actingFor)))
                          .in[R] when !id.isEmpty
        } yield {
          println(s"id: $id")
          if (notRequiredToComplete)
            Left(NoNeedToComplete)
          else
            Right(Some(AboutYou(false, alive, id, deceasedBefore, employmentStatus, personName)))
        }
      }
  } yield (ret)

}
