import sbt.Def
import org.portablescala.sbtplatformdeps.PlatformDepsGroupArtifactID
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.librarymanagement._

object Settings {

  object LibraryVersions {
    val cats                = "2.8.0"
    val catsEffect          = "3.3.14"
    val catsRetry           = "3.1.0"
    val circe               = "0.14.2"
    val circeGolden         = "0.3.0"
    val clue                = "0.23.1"
    val crystal             = "0.30.0"
    val discipline          = "1.5.1"
    val disciplineMUnit     = "1.0.9"
    val fs2                 = "3.2.11"
    val fs2Data             = "1.4.1"
    val geminiLocales       = "0.7.0"
    val http4s              = "0.23.14"
    val http4sDom           = "0.2.3"
    val log4Cats            = "2.4.0"
    val log4CatsLogLevel    = "0.3.1"
    val lucumaBC            = "0.4.0"
    val lucumaCore          = "0.45.0"
    val lucumaCatalog       = "0.25.0"
    val lucumaUI            = "0.39.0"
    val lucumaSchemas       = "0.33.0"
    val lucumaSSO           = "0.1.13"
    val monocle             = "3.1.0"
    val mouse               = "1.1.0"
    val mUnit               = "0.7.29"
    val mUnitCatsEffect     = "1.0.7"
    val reactAladin         = "0.23.0"
    val reactBeautifulDnD   = "0.4.2"
    val reactClipboard      = "1.5.1"
    val reactCommon         = "0.17.0"
    val reactDatepicker     = "0.4.1"
    val reactGridLayout     = "0.16.1"
    val reactHighcharts     = "0.4.4"
    val reactHotkeys        = "0.4.0"
    val reactResizable      = "0.9.0"
    val reactSemanticUI     = "0.16.0"
    val reactTable          = "0.7.4"
    val reactVirtuoso       = "0.2.2"
    val scalaJsReact        = "2.1.1"
    val pprint              = "0.7.3"
    val scalaJSSecureRandom = "1.0.0"
    var webAppUtil          = "2.0.0-RC2"
    var boopickle           = "1.4.0"

  }

  object Libraries {
    import LibraryVersions._

    private def deps(modules: PlatformDepsGroupArtifactID*)(version: String): Seq[ModuleID] =
      modules.map(_ % version)

    def In(configuration: Configuration)(dependencies: Seq[ModuleID]): Seq[ModuleID] =
      dependencies.map(_ % configuration)

    val SecureRandom = Def.setting(
      Seq(
        "org.scala-js" %%% "scalajs-java-securerandom" % scalaJSSecureRandom
      )
    )

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

    val ClueGenerator = Def.setting(
      deps(
        "edu.gemini" %%% "clue-generator"
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

    val FS2 = Def.setting(
      deps(
        "co.fs2" %%% "fs2-core"
      )(fs2)
    )

    val FS2Data = Def.setting(
      deps(
        "org.gnieh" %%% "fs2-data-csv"
      )(fs2Data)
    )

    val FS2IO = Def.setting(
      deps(
        "co.fs2" %%% "fs2-io"
      )(fs2)
    )

    val FS2Node = Def.setting(
      deps(
        "co.fs2" %%% "fs2-node"
      )(fs2)
    )

    val GeminiLocales = Def.setting(
      deps(
        "edu.gemini" %%% "gemini-locales"
      )(geminiLocales)
    )

    val Http4sDom = Def.setting(
      deps(
        "org.http4s" %%% "http4s-dom"
      )(http4sDom)
    )

    val Http4sCore = Def.setting(
      deps(
        "org.http4s" %%% "http4s-core",
        "org.http4s" %%% "http4s-circe"
      )(http4s)
    )

    val Log4Cats = Def.setting(
      Seq(
        "org.typelevel" %%% "log4cats-core"     % log4Cats,
        "com.rpiaggio"  %%% "log4cats-loglevel" % log4CatsLogLevel
      )
    )

    val LucumaBC = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-broadcast-channel"
      )(lucumaBC)
    )

    val LucumaCatalog = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-catalog"
      )(lucumaCatalog)
    )

    val LucumaAgs = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-ags"
      )(lucumaCatalog)
    )

    val LucumaCore = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core"
      )(lucumaCore)
    )

    val LucumaCoreTestKit = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-core-testkit"
      )(lucumaCore)
    )

    val LucumaSchemas = Def.setting(
      deps(
        "edu.gemini" %%% "lucuma-schemas"
      )(lucumaSchemas)
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

    val Monocle = Def.setting(
      deps(
        "dev.optics" %%% "monocle-core",
        "dev.optics" %%% "monocle-macro",
        "dev.optics" %%% "monocle-unsafe"
      )(monocle)
    )

    val MonocleLaw = Def.setting(
      deps(
        "dev.optics" %%% "monocle-law"
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

    val ReactBeautifulDnD = Def.setting(
      deps(
        "com.rpiaggio" %%% "scalajs-react-beautiful-dnd"
      )(reactBeautifulDnD)
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

    val ReactVirtuoso = Def.setting(
      deps(
        "io.github.toddburnside" %%% "scalajs-react-virtuoso"
      )(reactVirtuoso)
    )

    val ScalaJSReact = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "core-bundle-cb_io",
        "com.github.japgolly.scalajs-react" %%% "extra",
        "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3",
        "com.github.japgolly.scalajs-react" %%% "callback-ext-cats_effect"
      )(scalaJsReact)
    )

    val ScalaJSReactTest = Def.setting(
      deps(
        "com.github.japgolly.scalajs-react" %%% "test"
      )(scalaJsReact)
    )

    val PPrint = Def.setting(
      deps(
        "com.lihaoyi" %%% "pprint"
      )(pprint)
    )

    val ScalaWebAppUtil = Def.setting(
      deps(
        "com.github.japgolly.webapp-util" %%% "core",
        "com.github.japgolly.webapp-util" %%% "core-boopickle"
      )(webAppUtil)
    )

    val Boopickle = Def.setting(
      deps(
        "io.suzaku" %%% "boopickle"
      )(boopickle)
    )
  }
}
