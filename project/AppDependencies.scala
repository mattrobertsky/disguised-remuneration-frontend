import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val uniformVersion = "0738c5715071bc2bd68ab052ca4ae9219880106c-SNAPSHOT"

  val compile = Seq(

    "uk.gov.hmrc"             %% "govuk-template"           % "5.26.0-play-26",
    "uk.gov.hmrc"             %% "play-ui"                  % "7.27.0-play-26",
    "uk.gov.hmrc"             %% "bootstrap-play-26"        % "0.36.0",
    "com.luketebbs.uniform"   %% "interpreter-play25"       % uniformVersion,
    "com.luketebbs.uniform"   %% "govuk-widgets"            % uniformVersion
  )

  val test = Seq(
    "org.scalatest"           %% "scalatest"                % "3.0.4"                 % "test",
    "org.jsoup"               %  "jsoup"                    % "1.10.2"                % "test",
    "com.typesafe.play"       %% "play-test"                % current                 % "test",
    "org.pegdown"             %  "pegdown"                  % "1.6.0"                 % "test, it",
    "uk.gov.hmrc"             %% "service-integration-test" % "0.2.0"                 % "test, it",
    "org.scalatestplus.play"  %% "scalatestplus-play"       % "3.1.2"                 % "test, it"
  )

}
