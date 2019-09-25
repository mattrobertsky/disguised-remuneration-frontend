import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings
import uk.gov.hmrc.SbtArtifactory
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

val appName = "disguised-remuneration-frontend"

PlayKeys.playDefaultPort := 8844

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    // Semicolon-separated list of regexs matching classes to exclude
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*views.*;.*prod.*;.*test.*",
    ScoverageKeys.coverageExcludedFiles := "<empty>;.*BuildInfo.*;.*Routes.*;",
    ScoverageKeys.coverageMinimum := 80,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true
  )
}

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .settings(
    majorVersion                     := 0,
    libraryDependencies              ++= AppDependencies.compile ++ AppDependencies.test,
    scoverageSettings
  )
  .settings(publishingSettings: _*)
  .configs(IntegrationTest)
  .settings(integrationTestSettings(): _*)
  .settings(
    resolvers += Resolver.jcenterRepo,
    resolvers += Resolver.bintrayRepo("hmrc", "releases"),
//    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    scalacOptions -= "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    TwirlKeys.templateImports ++= Seq(
      "ltbs.uniform._",
      "ltbs.uniform.interpreters.playframework._"
    )
  )

libraryDependencies ++= Seq(
  ws
)

libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.2"),
  "com.github.ghik" %% "silencer-lib" % "1.4.2" % Provided
)

scalacOptions += "-P:silencer:pathFilters=.*views.*;.*routes"
resolvers += Resolver.sonatypeRepo("snapshots")