import sbt.Def
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.librarymanagement._

object Settings {

  object LibraryVersions {
    val cats            = "2.1.1"
    val catsEffect      = "2.1.3"
    val circe           = "0.13.0"
    val clue            = "0.0.7"
    val crystal         = "0.2.0"
    val diodeData       = "1.1.7"
    val diodeReact      = "1.1.7.160"
    val discipline      = "1.0.2"
    val disciplineMUnit = "0.2.0"
    val gspCoreTestKit  = "0.1.8"
    val gspMathTestKit  = "0.1.17"
    val gppUI           = "0.0.3"
    val log4Cats        = "1.0.1"
    val log4CatsLog4s   = "0.4.0-M1"
    val monocle         = "2.0.4"
    val mouse           = "0.24"
    val mUnit           = "0.7.7"
    val reactAladin     = "0.0.7"
    val reactCommon     = "0.7.1"
    val reactGridLayout = "0.4.0"
    val reactSemanticUI = "0.4.12"
    val reactSizeMe     = "0.3.4"
    val scalaJsReact    = "1.6.0"
  }

  object Libraries       {
    import LibraryVersions._

    private def deps(modules: PlatformDepsGroupArtifactID*)(version: String): Seq[ModuleID]        =
      modules.map(_ % version)

    def In(configuration:     Configuration)(dependencies:           Seq[ModuleID]): Seq[ModuleID] =
      dependencies.map(_ % configuration)

    val Cats = Def.setting(
      deps(
        "org.typelevel" %%% "cats-core"
      )(cats)
    )

    val CatsEffect = Def.setting(
      deps(
        "org.typelevel" %%% "cats-effect"
      )(catsEffect)
    )

    val Circe = Def.setting(
      deps(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser",
        "io.circe" %%% "circe-generic-extras"
      )(circe)
    )

    val Clue = Def.setting(
      deps(
        "com.rpiaggio" %%% "clue-core"
      )(clue)
    )

    val ClueScalaJS = Def.setting(
      deps(
        "com.rpiaggio" %%% "clue-scalajs"
      )(clue)
    )

    val Crystal = Def.setting(
      deps(
        "com.rpiaggio" %%% "crystal"
      )(crystal)
    )

    val DiodeData = Def.setting(
      deps(
        "io.suzaku" %%% "diode-data"
      )(diodeData)
    )

    val DiodeReact = Def.setting(
      deps(
        "io.suzaku" %%% "diode-react"
      )(diodeReact)
    )

    val Discipline = Def.setting(
      Seq(
        "org.typelevel" %%% "discipline-core"  % discipline,
        "org.typelevel" %%% "discipline-munit" % disciplineMUnit
      )
    )

    val GSPCoreTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "gsp-core-testkit"
      )(gspCoreTestKit)
    )

    val GSPMathTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "gsp-math-testkit"
      )(gspMathTestKit)
    )

    val GPPUI = Def.setting(
      deps(
        "edu.gemini" %%% "gpp-ui"
      )(gppUI)
    )

    val Log4Cats = Def.setting(
      Seq(
        "io.chrisdavenport" %%% "log4cats-core"  % log4Cats,
        "io.chrisdavenport" %%% "log4cats-log4s" % log4CatsLog4s
      )
    )

    val Monocle = Def.setting(
      deps(
        "com.github.julien-truffaut" %%% "monocle-core",
        "com.github.julien-truffaut" %%% "monocle-macro"
      )(monocle)
    )

    val MonocleLaw = Def.setting(
      deps(
        "com.github.julien-truffaut" %%% "monocle-law"
      )(monocle)
    )

    val Mouse = Def.setting(
      deps(
        "org.typelevel" %%% "mouse"
      )(mouse)
    )

    val MUnit = Def.setting(
      deps(
        "org.scalameta" %%% "munit",
        "org.scalameta" %%% "munit-scalacheck"
      )(mUnit)
    )

    val ReactAladin = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-aladin"
      )(reactAladin)
    )

    val ReactCommon = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "common"
      )(reactCommon)
    )

    val ReactGridLayout = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-grid-layout"
      )(reactGridLayout)
    )

    val ReactSizeMe = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-sizeme"
      )(reactSizeMe)
    )

    val ReactSemanticUI = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-semantic-ui"
      )(reactSemanticUI)
    )

    val ScalaJSReact = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "core",
        "com.github.japgolly.scalajs-react" %%% "extra",
        "com.github.japgolly.scalajs-react" %%% "ext-monocle-cats",
        "com.github.japgolly.scalajs-react" %%% "ext-cats"
      )(scalaJsReact)
    )

    val ScalaJSReactTest = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "test"
      )(scalaJsReact)
    )
  }
}
