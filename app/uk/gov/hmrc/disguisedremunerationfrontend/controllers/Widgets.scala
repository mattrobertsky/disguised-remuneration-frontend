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

package uk.gov.hmrc.disguisedremunerationfrontend.controllers

import cats.syntax.either._, cats.syntax.applicative._, cats.instances.list._
import cats.data.{Validated, NonEmptyList}
import java.time.LocalDate
import ltbs.uniform.TreeLike.ops._
import ltbs.uniform.common.web.FormField
import ltbs.uniform.interpreters.playframework.{Path}
import ltbs.uniform.{BigString, ErrorTree, Input, UniformMessages, ErrorMsg, BigStringTag}
import play.twirl.api.Html
import uk.gov.hmrc.disguisedremunerationfrontend.data.Scheme
import uk.gov.hmrc.disguisedremunerationfrontend.views

trait Widgets {

  implicit val twirlStringField = new FormField[String, Html] {
    def decode(out: Input): Either[ErrorTree, String] =
      out.valueAtRoot.flatMap(_.headOption).getOrElse("").asRight

    def encode(in: String): Input = Input.one(List(in))

    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: String =
        data.valueAtRoot.flatMap{_.headOption}.getOrElse("")
      views.html.uniform.string(key, existingValue, errors, messages)
    }
  }

  implicit val twirlBoolField = new FormField[Boolean, Html] {
    val True = true.toString.toUpperCase
    val False = false.toString.toUpperCase

    def decode(out: Input): Either[ErrorTree, Boolean] = {
      val root: Option[List[String]] = out.valueAtRoot
      root match {
        case None | Some(Nil) => Left(ErrorMsg("required").toTree)
        case Some(List(True)) => Right(true)
        case Some(List(False)) => Right(false)
        case _ => Left(ErrorMsg("bad.value").toTree)
      }
    }

    def encode(in: Boolean): Input = Input.one(List(in.toString))

    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val options = if (key.contains("about-you") || key.contains("user-employed")) List(False, True) else List(True, False)
      val existingValue: Option[String] = data.valueAtRoot.flatMap{_.headOption}
      views.html.uniform.radios(key,
        options,
        existingValue,
        errors,
        messages)
    }
  }

  implicit val twirlDateField = new FormField[LocalDate, Html] {

    import Scheme._

    def decode(out: Input): Either[ErrorTree, LocalDate] = {

      def intAtKey(key: String): String =
        out.valueAt(key).flatMap(x => x.headOption).getOrElse("")

      (
        intAtKey("year"),
        intAtKey("month"),
        intAtKey("day")
      ) match {
        case (y, m, d) if y.isEmpty && m.isEmpty && d.isEmpty =>
          ErrorTree.oneErr(ErrorMsg("date-empty")).asLeft
        case (_, m, d) if m.isEmpty && d.isEmpty =>
          Map(NonEmptyList.of(List("day"), List("month")) -> NonEmptyList.one(ErrorMsg("empty"))).asLeft
        case (y, _, d) if y.isEmpty && d.isEmpty =>
          Map(NonEmptyList.of(List("day"), List("month")) -> NonEmptyList.one(ErrorMsg("empty"))).asLeft
        case (y, m, _) if y.isEmpty && m.isEmpty =>
          Map(NonEmptyList.of(List("year"), List("month")) -> NonEmptyList.one(ErrorMsg("empty"))).asLeft
        case (y, _, _) if y.isEmpty =>
          Map(NonEmptyList.of(List("year")) -> NonEmptyList.one(ErrorMsg("empty"))).asLeft
        case (_, m, _) if m.isEmpty =>
          Map(NonEmptyList.of(List("month")) -> NonEmptyList.one(ErrorMsg("empty"))).asLeft
        case (_, _, d) if d.isEmpty =>
          Map(NonEmptyList.of(List("day")) -> NonEmptyList.one(ErrorMsg("empty"))).asLeft
        //        case (y, _, _) if !y.matches(yearRegex) => // TODO clarify why this was here (no message)
        //          ErrorTree.oneErr(ErrorMsg("year-incorrect")).prefixWith("year").asLeft
        case (y, m, d) => Either.catchOnly[java.time.DateTimeException] {
          LocalDate.of(y.toInt, m.toInt, d.toInt)
        }.leftMap(_ => ErrorTree.oneErr(ErrorMsg("not-a-date")))
      }
    }

    def encode(in: LocalDate): Input = Map(
      List("year") → in.getYear(),
      List("month") → in.getMonthValue(),
      List("day") → in.getDayOfMonth()
    ).mapValues(_.toString.pure[List])

    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      views.html.uniform.date(
        key,
        data,
        errors,
        messages
      )
    }
  }

}
