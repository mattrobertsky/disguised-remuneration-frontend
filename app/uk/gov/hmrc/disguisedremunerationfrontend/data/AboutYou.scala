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

import cats.data.NonEmptyList
import cats.implicits._
import ltbs.uniform.{::, Language, NilTypes, _}
import play.api.i18n.{Messages => _}
import uk.gov.hmrc.disguisedremunerationfrontend.controllers.EmploymentStatus

import scala.language.higherKinds

sealed trait AboutYou {
  def identification: Either[Nino, Utr]
  def completedBySelf: Boolean
}

case class AboutSelf(
    nino: String
) extends AboutYou {
  def identification: Either[Nino, Utr] = Left(nino)
  def completedBySelf: Boolean = true
}

case class AboutAnother(
    alive: Boolean,
    identification: Either[Nino, Utr],
    deceasedBefore: Option[Boolean],
    employmentStatus: Option[EmploymentStatus] = None,
    actingFor: String
) extends AboutYou {
  def completedBySelf: Boolean = false
}

object AboutYou {
  case object NoNeedToComplete
  // Move into utils
  lazy val nameRegex = """^[a-zA-Z',-. ]*$"""
  lazy val regExUTR = """^(?:[ \t]*(?:[a-zA-Z]{3})?\d[ \t]*){10}$"""
  lazy val regExNino = """^[ \t]*[A-Z,a-z]{1}[ \t]*[ \t]*[A-Z,a-z]{1}[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*""" +
    """[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*[ \t]*[0-9]{1}[ \t]*[A-D,a-d]{1}[ \t]*$"""
  type TellTypes = NoNeedToComplete.type :: NilTypes
  type AskTypes = Boolean :: String :: Either[Nino,Utr] :: EmploymentStatus :: NilTypes

  def validIdentifier[A <: Either[Nino,Utr]](input: A): Boolean = input.fold(validNino, validUtr)
  def validNino(nino: Nino):Boolean = nino.matches(regExNino)
  def validUtr(utr: Utr):Boolean = utr.matches(regExUTR)

  private def aboutAnotherProgram[F[_] : cats.Monad] (
    interpreter: Language[F, TellTypes, AskTypes]
  ): F[Either[NoNeedToComplete.type, AboutYou]] = {

    import interpreter._

    for {
      alive <- ask[Boolean]("user-deceased")
      employmentStatus <- ask[EmploymentStatus]("was-user-self-employed") when !alive
      deceasedBefore <- ask[Boolean]("aboutyou-deceasedbefore") when employmentStatus.contains(EmploymentStatus.Employed)
      r <- if(deceasedBefore.contains(true)) {
        tell[NoNeedToComplete.type ]("aboutyou-noloancharge", NoNeedToComplete)  >> Left(NoNeedToComplete).pure[F]
      } else {
        for {
          id <- ask[Either[Nino, Utr]]("about-scheme-user",
            validation = List(
              List(
                Rule.fromPred({
                  case Left(nino) => nino.nonEmpty
                  case _          => true
                },
                  (ErrorMsg("required"),
                  NonEmptyList.one(List("Left", "a")))),
                Rule.fromPred({
                  case Right(utr) => utr.nonEmpty
                  case _          => true
                },
                  (ErrorMsg("required"),
                  NonEmptyList.one(List("Right", "b"))))
              ),
              List(
                Rule.fromPred({
                                case Left(nino) => nino.matches(regExNino)
                                case _          => true
                              },
                              (ErrorMsg("format"),
                               NonEmptyList.one(List("Left", "a")))),
                Rule.fromPred({
                                case Right(utr) => utr.matches(regExUTR)
                                case _          => true
                              },
                              (ErrorMsg("format"),
                               NonEmptyList.one(List("Right", "b"))))
                  )
                )
          )
          personName <- ask[String](
            "confirm-about-scheme-user",
            validation = List(
              List(
                Rule.fromPred(
                  x => x.matches(nonEmptyStringRegex),
                  (ErrorMsg("required"), NonEmptyList.one(Nil))
                )
              ),
              List(
                Rule.fromPred(
                  x ⇒ x.matches(nameRegex),
                  (ErrorMsg("format"), NonEmptyList.one(Nil))
                )
              ),
              List(
                Rule.fromPred(
                  x ⇒ x.length <= 50,
                  (ErrorMsg("limit"), NonEmptyList.one(Nil))
                ))
            )
          )
        } yield Right(AboutAnother(
          alive,
          id,
          deceasedBefore,
          employmentStatus,
          personName
        ))
      }
    } yield r
  }

  private def aboutSelfProgram[F[_]: cats.Monad](
    nino: Option[Nino], // TODO these should be used
    utr: Option[Utr],   // TODO ditto
    interpreter: Language[F, TellTypes, AskTypes]
  ): F[Either[NoNeedToComplete.type, AboutYou]] = {
    import interpreter._

    val i = nino match {
      case Some(n) => n.pure[F]
      case None =>
        ask[Nino]("your-ni-no",
          validation =
            List(
              List(
                Rule.fromPred(
                  nino => nino.nonEmpty,
                  (ErrorMsg("required"), NonEmptyList.one(Nil))
                )
              ),
              List(
                Rule.fromPred(
                  validNino,
                  (ErrorMsg("format"), NonEmptyList.one(Nil))
                )
              )
            )
        )
    }
    i.map { x =>
      AboutSelf(x).asRight[NoNeedToComplete.type]
    }
  }

  def aboutYouProgram[F[_]: cats.Monad](
      interpreter: Language[F, TellTypes, AskTypes],
      nino: Option[Nino] = None,
      utr: Option[Utr] = None
  ): F[Either[NoNeedToComplete.type, AboutYou]] = {
    import interpreter._

    for {
      aboutAnother <- ask[Boolean]("about-you")
      aboutBranch <- if (aboutAnother) aboutAnotherProgram(interpreter)
      else aboutSelfProgram(nino, utr, interpreter)
    } yield aboutBranch
  }
}
