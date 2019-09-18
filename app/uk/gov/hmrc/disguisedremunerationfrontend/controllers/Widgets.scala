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

import cats.implicits._
import cats.data.{Validated, NonEmptyList}
import java.time.LocalDate
import ltbs.uniform.TreeLike.ops._
import ltbs.uniform.common.web.FormField
import ltbs.uniform.interpreters.playframework.{Path}
import ltbs.uniform.{BigString, ErrorTree, Input, UniformMessages, ErrorMsg, BigStringTag, InputOps, RichInput}
import play.twirl.api.Html
import uk.gov.hmrc.disguisedremunerationfrontend.data.{Scheme, Address}
import uk.gov.hmrc.disguisedremunerationfrontend.views
import collection.immutable.ListMap

trait Widgets extends InputOps {

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

      def intAtKey(key: String): Validated[List[String], Int] =
        Validated.fromOption(
          out.valueAt(key).flatMap{_.find(_.trim.nonEmpty)},
          List(key)
        ).andThen{
          x => Validated.catchOnly[NumberFormatException](x.toInt).leftMap(_ => List(key))
        }

        (
          intAtKey("year"),
          intAtKey("month"),
          intAtKey("day")
        ).tupled
        .leftMap{x => ErrorMsg(x.reverse.mkString("-and-") + ".empty").toTree}
        .toEither
        .flatMap{ case (y,m,d) =>
          Either.catchOnly[java.time.DateTimeException]{
            LocalDate.of(y,m,d)
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

  implicit def twirlAddressField[T](
    implicit gen: shapeless.LabelledGeneric.Aux[Address,T],
    ffhlist: FormField[T, Html]
  )= new FormField[Address, Html] {

    import Scheme._

    def decode(out: Input): Either[ErrorTree, Address] = {
      import cats.implicits._

      val postCodeRegex = """([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\s?[0-9][A-Za-z]{2}|.{0})"""

      def standardValidation: String => Validated[ErrorTree, String] =
        maxLength(40)(_) andThen matchesRegex("""^[a-zA-Z0-9',-./ ]*$""")

      (
        out.stringSubField("line1", nonEmpty(_) andThen standardValidation),
        out.stringSubField("line2", standardValidation(_)),
        out.stringSubField("town", nonEmpty(_) andThen standardValidation),
        out.stringSubField("county", standardValidation(_)),
        out.stringSubField("postcode", nonEmpty(_) andThen matchesRegex(postCodeRegex))
      ).mapN(Address).toEither
    }

    def encode(in: Address): Input =
      ffhlist.encode(gen.to(in))

    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      views.html.uniform.address(
        key,
        data,
        errors,
        messages
      )
    }
  }

}
