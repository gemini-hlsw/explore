import org.scalajs.linker.interface.ModuleSplitStyle
import sbtcrossproject.crossProject
import sbtcrossproject.CrossType
import Settings.Libraries._

val reactJS                   = "16.13.1"
val FUILess                   = "2.8.7"
val kindProjectorVersion = "0.11.3"


addCommandAlias(
  "quickTest",
  "modelJVM/test"
)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports"
)

addCommandAlias(
  "graphQLGen",
  "; scalafix GraphQLGen"
)

addCommandAlias(
  "graphQLCheck",
  "; scalafix GraphQLGen --check"
)

inThisBuild(
  Seq(
    homepage := Some(url("https://github.com/gemini-hlsw/explore")),
    addCompilerPlugin(
      ("org.typelevel"                    % "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    ),
    description := "Explore",
    scalacOptions += "-Ymacro-annotations",
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    scalafixDependencies += "edu.gemini" %% "clue-generator" % Settings.LibraryVersions.clue,
    scalafixScalaBinaryVersion := "2.13"
  ) ++ lucumaPublishSettings
)

val stage = taskKey[Unit]("Prepare static files to deploy to Heroku")

// For simplicity, the build's stage only deals with the explore app.
stage := {
  val jsFiles = (explore / Compile / fullLinkJS).value
  if (sys.env.getOrElse("POST_STAGE_CLEAN", "false").equals("true")) {
    println("Cleaning up...")
    // Remove sbt-scalajs-bundler directory, which includes node_modules.
    // val bundlerDir       =
    //   (explore / Compile / fullOptJS / artifactPath).value.getParentFile.getParentFile
    // sbt.IO.delete(bundlerDir)
    // Remove coursier cache
    val coursierCacheDir = csrCacheDirectory.value
    sbt.IO.delete(coursierCacheDir)
  }
}

lazy val root = project
  .in(file("."))
  .settings(name := "explore-root")
  .settings(commonSettings: _*)
  .aggregate(model.jvm, model.js, common, explore)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("model"))
  .settings(commonSettings: _*)
  .settings(commonLibSettings: _*)
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .jvmSettings(commonJVMSettings)

val curTime = System.currentTimeMillis()

lazy val common = project
  .in(file("common"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(
    Test / test := {},
    libraryDependencies ++=
      LucumaSSO.value ++
        LucumaBC.value ++
        LucumaCatalog.value ++
        ReactGridLayout.value ++
        ReactClipboard.value ++
        ReactCommon.value ++
        ReactTable.value,
    buildInfoKeys := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit
    ),
    buildInfoKeys ++= {
      Seq(BuildInfoKey.action("buildTime")(curTime))
    },
    buildInfoPackage := "explore"
  )
  .enablePlugins(ScalaJSPlugin, BuildInfoPlugin)
  .dependsOn(model.js)

lazy val explore: Project = project
  .in(file("explore"))
  .settings(commonSettings: _*)
  .settings(commonJsLibSettings: _*)
  .settings(commonES: _*)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Test / test := {},
    libraryDependencies ++=
      GeminiLocales.value ++
        ReactDatepicker.value ++
        ReactAladin.value ++
        ReactAtlasKitTree.value ++
        ReactCommon.value ++
        ReactGridLayout.value ++
        ReactHighcharts.value ++
        ReactResizable.value
  )
  .dependsOn(model.js, common)

lazy val commonSettings = lucumaGlobalSettings ++ Seq(
  // don't publish anything
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  Keys.`package` := file("")
)

lazy val commonLibSettings = Seq(
  libraryDependencies ++=
    LucumaCore.value ++
      Cats.value ++
      Mouse.value ++
      CatsEffect.value ++
      CatsRetry.value ++
      Monocle.value ++
      Circe.value ++
      Crystal.value ++
      Sttp.value ++
      Clue.value ++
      In(Test)(
        MUnit.value ++
          Discipline.value ++
          MonocleLaw.value ++
          LucumaCoreTestKit.value
      ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val commonJVMSettings = Seq(
  libraryDependencies ++=
    In(Test)(
      CirceGolden.value
    )
)

lazy val commonJsLibSettings = lucumaScalaJsSettings ++ commonLibSettings ++ Seq(
  libraryDependencies ++=
    ScalaJSReact.value ++
      ReactSemanticUI.value ++
      ClueScalaJS.value ++
      LucumaUI.value ++
      Log4Cats.value ++
      In(Test)(
        ScalaJSReactTest.value
      ),
  dependencyOverrides ++= ScalaJSReact.value
)

lazy val commonES = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
  Compile / fastLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules)),
  Compile / fullLinkJS / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(ModuleSplitStyle.FewestModules)),
)
