import sbt.Def
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.librarymanagement._

object Settings {

  object LibraryVersions {
    val cats              = "2.6.1"
    val catsEffect        = "3.1.1"
    val catsRetry         = "3.0.0"
    val circe             = "0.14.1"
    val circeGolden       = "0.3.0"
    val clue              = "0.16.0"
    val crystal           = "0.15.1"
    val discipline        = "1.1.5"
    val disciplineMUnit   = "1.0.9"
    val fs2               = "3.0.6"
    val fs2Data           = "1.0.0"
    val geminiLocales     = "0.6.0"
    val log4Cats          = "2.1.1"
    val log4CatsLogLevel  = "0.3.0"
    val lucumaCore        = "0.10.1"
    val lucumaCatalog     = "0.4.1"
    val lucumaUI          = "0.15.1"
    val lucumaSSO         = "0.0.9"
    val lucumaBC          = "0.2.4"
    val monocle           = "2.1.0"
    val mouse             = "1.0.4"
    val mUnit             = "0.7.27"
    val mUnitCatsEffect   = "1.0.5"
    val reactAladin       = "0.5.1"
    val reactAtlasKitTree = "0.4.0"
    val reactClipboard    = "1.4.3"
    val reactCommon       = "0.11.3"
    val reactDatepicker   = "0.2.2"
    val reactGridLayout   = "0.12.0"
    val reactHighcharts   = "0.3.4"
    val reactHotkeys      = "0.3.1"
    val reactResizable    = "0.4.4"
    val reactSemanticUI   = "0.11.1"
    val reactTable        = "0.1.4"
    val scalaJsReact      = "1.7.7"
    val sttp              = "3.3.11"
    val pprint            = "0.6.6"
  }

  object Libraries {
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

    val CatsRetry = Def.setting(
      deps(
        "com.github.cb372" %%% "cats-retry"
      )(catsRetry)
    )

    val Circe = Def.setting(
      deps(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser",
        "io.circe" %%% "circe-generic-extras"
      )(circe)
    )

    val CirceGolden = Def.setting(
      Seq(
        "io.circe" %%% "circe-golden" % circeGolden
      )
    )

    val Clue = Def.setting(
      deps(
        "edu.gemini" %%% "clue-core"
      )(clue)
    )

    val ClueScalaJS = Def.setting(
      deps(
        "edu.gemini" %%% "clue-scalajs"
      )(clue)
    )

    val Crystal = Def.setting(
      deps(
        "com.rpiaggio" %%% "crystal"
      )(crystal)
    )

    val Discipline = Def.setting(
      Seq(
        "org.typelevel" %%% "discipline-core"  % discipline,
        "org.typelevel" %%% "discipline-munit" % disciplineMUnit
      )
    )

    val GeminiLocales = Def.setting(
      deps(
        "edu.gemini" %%% "gemini-locales"
      )(geminiLocales)
    )

    val FS2IO = Def.setting(
      deps(
        "co.fs2" %%% "fs2-io"
      )(fs2)
    )

    val FS2Data = Def.setting(
      deps(
        "org.gnieh" %%% "fs2-data-csv"
      )(fs2Data)
    )

    val Log4Cats = Def.setting(
      Seq(
        "org.typelevel" %%% "log4cats-core"     % log4Cats,
        "com.rpiaggio"  %%% "log4cats-loglevel" % log4CatsLogLevel
      )
    )

    val LucumaCore = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core"
      )(lucumaCore)
    )

    val LucumaCatalog = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-catalog"
      )(lucumaCatalog)
    )

    val LucumaCoreTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core-testkit"
      )(lucumaCore)
    )

    val LucumaUI = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-ui"
      )(lucumaUI)
    )

    val LucumaSSO = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-sso-frontend-client"
      )(lucumaSSO)
    )

    val LucumaBC = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-broadcast-channel"
      )(lucumaBC)
    )

    val Monocle = Def.setting(
      deps(
        "com.github.julien-truffaut" %%% "monocle-core",
        "com.github.julien-truffaut" %%% "monocle-state",
        "com.github.julien-truffaut" %%% "monocle-macro",
        "com.github.julien-truffaut" %%% "monocle-unsafe"
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

    val MUnitCatsEffect = Def.setting(
      deps(
        "org.typelevel" %%% "munit-cats-effect-3"
      )(mUnitCatsEffect)
    )

    val ReactAladin = Def.setting(
      deps(
        "edu.gemini" %%% "react-aladin"
      )(reactAladin)
    )

    val ReactAtlasKitTree = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-atlaskit-tree"
      )(reactAtlasKitTree)
    )

    val ReactClipboard = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-clipboard"
      )(reactClipboard)
    )

    val ReactCommon = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "common",
        "io.github.cquiroz.react" %%% "cats"
      )(reactCommon)
    )

    val ReactDatepicker = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-datepicker"
      )(reactDatepicker)
    )

    val ReactGridLayout = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-grid-layout"
      )(reactGridLayout)
    )

    val ReactHighcharts = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-highcharts"
      )(reactHighcharts)
    )

    val ReactHotkeys = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-hotkeys"
      )(reactHotkeys)
    )

    val ReactResizable = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-resizable"
      )(reactResizable)
    )

    val ReactSemanticUI = Def.setting(
      deps(
        "io.github.cquiroz.react" %%% "react-semantic-ui"
      )(reactSemanticUI)
    )

    val ReactTable = Def.setting(
      deps(
        "io.github.toddburnside" %%% "scalajs-react-table"
      )(reactTable)
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

    val Sttp = Def.setting(
      deps(
        "com.softwaremill.sttp.client3" %%% "core",
        "com.softwaremill.sttp.client3" %%% "circe",
        "com.softwaremill.sttp.client3" %%% "cats"
      )(sttp)
    )

    val PPrint = Def.setting(
      deps(
        "com.lihaoyi" %%% "pprint"
      )(pprint)
    )
  }
}
