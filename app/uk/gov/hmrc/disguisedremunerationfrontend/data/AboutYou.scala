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
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.EmploymentStatus
import cats.implicits._

sealed trait AboutYou {
  def identification: Either[Nino, Utr]
  def completedBySelf: Boolean
}

case class AboutSelf (
  nino: String
) extends AboutYou {
  def identification: Either[Nino, Utr] = Left(nino)
  def completedBySelf: Boolean = true  
}

case class AboutAnother (
  alive: Boolean,
  identification: Either[Nino, Utr],
  deceasedBefore: Option[Boolean],
  employmentStatus: Option[EmploymentStatus] = None,
  actingFor: String
) extends AboutYou {
  def completedBySelf: Boolean = false
}

object AboutYou {

  // Move into utils
  lazy val nameRegex = """^[a-zA-Z',-. ]*$"""
  lazy val regExUTR = """^(?:[ \t]*(?:[a-zA-Z]{3})?\d[ \t]*){10}$"""
  lazy val regExNino = """^[ \t]*[A-Z,a-z]{1}[ \t]*[ \t]*[A-Z,a-z]{1}[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*""" +
    """[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*[A-D,a-d]{1}[ \t]*$"""

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
  ](default: Option[AboutYou], nino: Option[Nino], utr: Option[Utr]): Eff[R, Either[Error,AboutYou]] = {

    def aboutAnotherProgram(localDefault: Option[AboutAnother]): Eff[R, Either[Error,AboutYou]] =
      for {
        alive            <- ask[Boolean]("user-deceased")
                              .defaultOpt(localDefault.map(_.alive))
        employmentStatus <- ask[EmploymentStatus]("was-user-self-employed")
                              .defaultOpt(localDefault.flatMap(_.employmentStatus))
                              .in[R] when !alive
        deceasedBefore   <- ask[Boolean]("aboutyou-deceasedbefore")
                              .defaultOpt(localDefault.flatMap(_.deceasedBefore))
                              .in[R] when employmentStatus == Some(EmploymentStatus.Employed)
        r                <- if (deceasedBefore == Some(true)) { 
          tell[Unit]("aboutyou-noloancharge")(()).in[R] >> Eff.pure(Left(NoNeedToComplete))
        } else {
          for {
            id <- ask[Either[Nino,Utr]]("about-scheme-user")
              .defaultOpt(localDefault.map(_.identification))
              .validating(
                "nino-format", {
                  case Left(nino) => nino.matches(regExNino)
                  case _ => true
                }
              ).validating(
                "utr-format",
                {
                  case Left(nino) => true
                  case Right(utr) => utr.matches(regExUTR)
                }
              )
              .in[R]
            personName <- ask[String]("confirm-about-scheme-user")
            .defaultOpt(localDefault.map(_.actingFor))
              .validating(
                "format",
                name => name.matches(nameRegex)
              )
              .validating(
                "limit",
                name => name.length <= 50
              )

            .in[R]
          } yield Right(AboutAnother(
            alive,
            id,
            deceasedBefore,
            employmentStatus,
            personName
          ))
        }
      } yield r

    def aboutSelfProgram(default: Option[AboutSelf]): Eff[R, Either[Error,AboutYou]] = {
      val i: Eff[R, String] = nino match {
        case Some(n) => Eff.pure(n)
        case None    => ask[Nino]("your-ni-no")
          .defaultOpt(default.map{_.nino})
          .validating(
            "format",
              ni => ni.matches(regExNino)
          )
      }
      i.map{x => AboutSelf(x).asRight[Error]}
    }

    ask[Boolean]("about-you")
      .defaultOpt(default.map{_.isInstanceOf[AboutAnother]}) >>= {
        if (_) aboutAnotherProgram(
          default.collect{case x: AboutAnother => x}
        ) else aboutSelfProgram(
          default.collect{case x: AboutSelf => x}
        )
      }

  }
}
