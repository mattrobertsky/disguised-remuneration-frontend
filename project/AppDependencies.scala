import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val uniformVersion = "0.4.5"

  val compile = Seq(

    "uk.gov.hmrc"             %% "govuk-template"           % "5.26.0-play-26",
    "uk.gov.hmrc"             %% "play-ui"                  % "7.27.0-play-26",
    "uk.gov.hmrc"             %% "bootstrap-play-26"        % "0.37.0",
    "com.beachape"            %% "enumeratum-play-json"     % "1.5.13",
    "com.luketebbs.uniform"   %% "interpreter-play26"       % uniformVersion,
    "uk.gov.hmrc"             %% "mongo-caching"            % "6.1.0-play-26",
    "uk.gov.hmrc"             %% "play-reactivemongo"       % "6.4.0",
    "uk.gov.hmrc"             %% "simple-reactivemongo"     % "7.12.0-play-26",
    "uk.gov.hmrc"             %% "auth-client"              % "2.20.0-play-26"
  )

  val test = Seq(
    "org.scalatest"           %% "scalatest"                % "3.0.4"                 % "test",
    "org.jsoup"               %  "jsoup"                    % "1.10.2"                % "test",
    "com.typesafe.play"       %% "play-test"                % current                 % "test",
    "org.pegdown"             %  "pegdown"                  % "1.6.0"                 % "test, it",
    "uk.gov.hmrc"             %% "service-integration-test" % "0.2.0"                 % "test, it",
    "org.scalatestplus.play"  %% "scalatestplus-play"       % "3.1.2"                 % "test, it",
    "com.luketebbs.uniform"   %% "interpreter-logictable"   % uniformVersion          % "test"
  )

}
