package explore.targets

import lucuma.core.enum.MagnitudeBand
import reactST.reactTable.TableDef
import lucuma.core.model.SiderealTarget
import japgolly.scalajs.react.vdom.html_<^._
import cats.Order._
import reactST.reactTable.Plugin
import lucuma.core.math.Declination
import lucuma.ui.optics._
import lucuma.core.model.Target
import lucuma.core.math.MagnitudeValue
import cats.syntax.all._
import lucuma.core.math.Epoch
import explore.model.formats._
import explore.model.conversions._
import lucuma.core.math.Parallax

object TargetColumns {

  val baseColNames: Map[String, String] = Map(
    "type" -> " ",
    "name" -> "Name"
  )

  val siderealColNames: Map[String, String] = Map(
    "ra"           -> "RA",
    "dec"          -> "Dec",
    "priority"     -> "Priority",
    "count"        -> "Count",
    "observations" -> "Observations",
    "epoch"        -> "Epoch",
    "pmra"         -> "µ RA",
    "pmdec"        -> "µ Dec",
    "rv"           -> "RV",
    "z"            -> "z",
    "cz"           -> "cz",
    "parallax"     -> "Parallax",
    "morphology"   -> "Morphology",
    "sed"          -> "SED"
  ) ++ MagnitudeBand.all.map(m => (m.shortName + "mag", m.shortName + "Mag")).toMap

  val allColNames: Map[String, String] = baseColNames ++ siderealColNames

  class SiderealColBuilder[D, Plugins, Layout](
    tableDef:          TableDef[D, Plugins, Layout],
    getSiderealTarget: D => Option[SiderealTarget]
  )(implicit ev:       Plugins <:< Plugin.SortBy.Tag) {

    def column[V](id: String, accessor: SiderealTarget => V) =
      tableDef
        .Column(id, getSiderealTarget.andThen(_.map(accessor)))
        .setHeader(siderealColNames(id))

    val columns =
      List(
        //
        // TODO Move to a new BaseColBuilder class
        //
        // column("type", _ => ())
        //   .setCell(_ => Icons.Star)
        //   .setWidth(30),
        // column("name", TargetWithId.name.get)
        //   .setCell(cell => cell.value.toString)
        //   .setSortByFn(_.toString),
        column(
          "ra",
          SiderealTarget.baseRA.get
        ).setCell(cell =>
          TruncatedRA.rightAscension.get
            .andThen(ValidFormatInput.truncatedRA.reverseGet)(cell.value)
        ).setSortByAuto,
        column[Declination](
          "dec",
          SiderealTarget.baseDec.get
        ).setCell(cell =>
          TruncatedDec.declination.get
            .andThen(ValidFormatInput.truncatedDec.reverseGet)(cell.value)
        ).setSortByAuto,
        column("priority", _ => "")
      ) ++
        MagnitudeBand.all.map(band =>
          column(
            band.shortName + "mag",
            t => Target.magnitudes.get(t).get(band).map(_.value)
          ).setCell(_.value.map(MagnitudeValue.fromString.reverseGet).orEmpty).setSortByAuto
        ) ++
        List(
          column("epoch", SiderealTarget.epoch.get)
            .setCell(cell =>
              s"${cell.value.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(cell.value)}"
            )
            .setSortByAuto,
          column("pmra", SiderealTarget.properMotionRA.getOption)
            .setCell(
              _.value.map(pmRAFormat.reverseGet).orEmpty
            )
            .setSortByAuto,
          column("pmdec", SiderealTarget.properMotionDec.getOption)
            .setCell(_.value.map(pmDecFormat.reverseGet).orEmpty)
            .setSortByAuto,
          column("rv", SiderealTarget.radialVelocity.get)
            .setCell(_.value.map(formatRV.reverseGet).orEmpty)
            .setSortByAuto,
          column("z", (SiderealTarget.radialVelocity.get _).andThen(rvToRedshiftGet))
            .setCell(_.value.map(formatZ.reverseGet).orEmpty)
            .setSortByAuto,
          column("cz", (SiderealTarget.radialVelocity.get _).andThen(rvToARVGet))
            .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
            .setSortByAuto,
          column("parallax", SiderealTarget.parallax.get)
            .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
            .setSortByAuto,
          column("morphology", _ => ""),
          column("sed", _ => "")
        )
  }
}
