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

package uk.gov.hmrc.disguisedremunerationfrontend.data.render

import ltbs.uniform.web.Messages
import play.twirl.api.Html

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

object RenderHtmlTemplate {

  def generateStartTag(tagName : String, attributesMap : Map[String, String]) : String = {

    val attributeList = attributesMap.foldLeft(""){
      (acc, iter) => {
        acc + iter._1 + " = " + "\"" + iter._2 + "\"" + " "
      }
    }

    val builder = StringBuilder.newBuilder
    builder.append("<" + tagName + " ")
    builder.append(attributeList)
    builder.append(">")
    builder.append("\n")

    builder.mkString

  }

  def generateEndTag(tagName : String) : String = {

    "</" + tagName + "> \n"

  }

  def generateCompletedByHtml(messages : Messages) : String = {

    ""

  }

  def generateIdentityHtml(messages : Messages) : String = {
    concatAll(

    generateStartTag("div", Map("class" -> "govuk-radios govuk-radios--conditional", "data-module" -> "radios")),

        generateStartTag("div", Map("class" -> "govuk-radios__item_nino")),
          generateStartTag("input", Map("class" -> "govuk-radios__input", "id" -> "aboutyou-identity-conditional-1", "name" -> "aboutyou-identity", "type" -> "radio", "value" -> "Left", "data-target" -> "conditional-aboutyou-identity-conditional-1")),
            generateStartTag("label", Map("class" -> "govuk-label govuk-radios__label", "for" -> "aboutyou-identity-conditional-1")),
              "The person's National Insurance number",
                generateStartTag("span", Map("class" -> "govuk-hint", "id" -> "aboutyou-identity.Left.a-hint")),
                  messages.get("aboutyou-identity.Left.a.heading").getOrElse(""),
                generateEndTag("span"),
            generateEndTag("label"),
          generateEndTag("input"),
        generateEndTag("div"),

        generateStartTag("div", Map("class" -> "govuk-radios__conditional", "id" -> "conditional-aboutyou-identity-conditional-1")),
          generateStartTag("div", Map("class" -> "govuk-form-group")),
            generateStartTag("label", Map("class" -> "govuk-label", "for" -> "aboutyou-identity.Left.a")),
              messages.get("aboutyou-identity.Left.a.label").getOrElse(""),
            generateEndTag("label"),
            generateStartTag("input", Map("class" -> "govuk-input govuk-!-width-one-third", "id" -> "aboutyou-identity.Left.a", "name" -> "aboutyou-identity.Left.a", "type" -> "text")),
            generateEndTag("input"),
          generateEndTag("div"),
        generateEndTag("div"),


      generateStartTag("div", Map("class" -> "govuk-radios__item_utr")),
        generateStartTag("input", Map("class" -> "govuk-radios__input", "id" -> "aboutyou-identity-conditional-2", "name" -> "aboutyou-identity", "type" -> "radio", "value" -> "Right", "data-target" -> "conditional-aboutyou-identity-conditional-2")),
          generateStartTag("label", Map("class" -> "govuk-label govuk-radios__label", "for" -> "aboutyou-identity-conditional-2")),
             "The person's Self Assessment Unique Taxpayer Reference (UTR)",
                generateStartTag("span", Map("class" -> "govuk-hint", "id" -> "aboutyou-identity.Right.b-hint")),
                  messages.get("aboutyou-identity.Right.b.heading").getOrElse(""),
                generateEndTag("span"),
          generateEndTag("label"),
        generateEndTag("input"),
      generateEndTag("div"),

      generateStartTag("div", Map("class" -> "govuk-radios__conditional", "id" -> "conditional-aboutyou-identity-conditional-2")),
        generateStartTag("div", Map("class" -> "govuk-form-group")),
          generateStartTag("label", Map("class" -> "govuk-label", "for" -> "aboutyou-identity.Right.b")),
            messages.get("aboutyou-identity.Right.label").getOrElse(""),
          generateEndTag("label"),
          generateStartTag("input", Map("class" -> "govuk-input govuk-!-width-one-third", "id" -> "aboutyou-identity.Right.b", "name" -> "aboutyou-identity.Right.b", "type" -> "text")),
          generateEndTag("input"),
        generateEndTag("div"),
      generateEndTag("div"),

    generateEndTag("div")

    )


  }

  def concatAll(strings: String*) = {

    val builder = StringBuilder.newBuilder
    strings.foreach(x => builder.append(x))

    builder.mkString

  }




}