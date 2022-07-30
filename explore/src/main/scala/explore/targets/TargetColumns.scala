// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.conversions._
import explore.model.display._
import explore.model.enums.SourceProfileType
import explore.model.formats._
import explore.optics.all._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Band
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion.Dec
import lucuma.core.math.ProperMotion.RA
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional._
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Target
import lucuma.core.syntax.display._
import lucuma.core.util.Display
import lucuma.ui.syntax.all.given
import reactST.reactTable._
import reactST.reactTable.facade.cell.CellProps

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
  ) ++ Band.all.map(m => (m.tag + "mag", m.shortName)).toMap

  val allColNames: Map[String, String] = baseColNames ++ siderealColNames

  // TODO In Scala 3, we should use trait parameters.

  trait BaseColBuilder[D, Plugins, Layout] {
    val tableDef: TableDef[D, Plugins, Layout]
    val getTarget: D => Option[Target]
    implicit val sortByEv: Plugins <:< Plugin.SortBy.Tag

    def baseColumn[V](
      id:       String,
      accessor: Target => V
    ): ColumnValueOptions[D, Option[V], Plugins] =
      tableDef
        .Column(id, getTarget.andThen(_.map(accessor)))
        .setHeader(baseColNames(id))

    val baseColumns =
      List(
        baseColumn("type", _ => ())
          .setCell((_: CellProps[D, Option[Unit], Plugins]) => Icons.Star)
          .setWidth(30),
        baseColumn("name", Target.name.get)
          .setCell((x: CellProps[D, Option[NonEmptyString], Plugins]) =>
            x.value.map(_.toString).orEmpty
          )
        // .setSortByFn(_.toString)
      )
  }

  trait SiderealColBuilder[D, Plugins, Layout] {
    val tableDef: TableDef[D, Plugins, Layout]
    val getSiderealTarget: D => Option[Target.Sidereal]
    implicit val sortByEv: Plugins <:< Plugin.SortBy.Tag

    def siderealColumnOpt[V](
      id:       String,
      accessor: Target.Sidereal => Option[V]
    ): ColumnValueOptions[D, Option[V], Plugins] =
      tableDef
        .Column(id, getSiderealTarget.andThen(_.flatMap(accessor)))
        .setHeader(siderealColNames(id))

    def siderealColumn[V](
      id:       String,
      accessor: Target.Sidereal => V
    ): ColumnValueOptions[D, Option[V], Plugins] =
      siderealColumnOpt(id, accessor.andThen(_.some))

    /** Display measure without the uncertainty */
    def displayWithoutError[N](measure: Measure[N])(implicit d: Display[N]): VdomNode =
      <.div(
        <.span(measure.value.shortName),
        <.span(ExploreStyles.UnitsTableLabel, measure.units.shortName.replace(" mag", ""))
      )

    val siderealColumns =
      List(
        siderealColumn("ra", Target.Sidereal.baseRA.get)
          .setCell((x: CellProps[D, Option[RightAscension], Plugins]) =>
            x.value
              .map(MathValidators.truncatedRA.reverseGet)
              .orEmpty
          )
          .setSortByAuto,
        siderealColumn("dec", Target.Sidereal.baseDec.get)
          .setCell((x: CellProps[D, Option[Declination], Plugins]) =>
            x.value
              .map(MathValidators.truncatedDec.reverseGet)
              .orEmpty
          )
          .setSortByAuto
        // siderealColumn("priority", _ => "").setCell(_ =>
        //   ""
        // ) // TODO IS this a property of the target, or a property of the target in the observation???
      ) ++
        Band.all.map(band =>
          siderealColumnOpt(
            band.tag + "mag",
            t => targetBrightnesses.get(t).flatMap(_.get(band))
          )
            .setCell(_.value.map(displayWithoutError(_)(displayBrightness)))
            .setDisableSortBy(true) // We cannot sort since there may be different units.
        ) ++
        List(
          siderealColumn("epoch", Target.Sidereal.epoch.get)
            .setCell((x: CellProps[D, Option[Epoch], Plugins]) =>
              x.value
                .map(value =>
                  s"${value.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(value)}"
                )
                .orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("pmra", Target.Sidereal.properMotionRA.getOption)
            .setCell((x: CellProps[D, Option[RA], Plugins]) =>
              x.value.map(pmRAFormat.reverseGet).orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("pmdec", Target.Sidereal.properMotionDec.getOption)
            .setCell((x: CellProps[D, Option[Dec], Plugins]) =>
              x.value.map(pmDecFormat.reverseGet).orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("rv", Target.Sidereal.radialVelocity.get)
            .setCell((x: CellProps[D, Option[RadialVelocity], Plugins]) =>
              x.value.map(formatRV.reverseGet).orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("z", (Target.Sidereal.radialVelocity.get _).andThen(rvToRedshiftGet))
            .setCell((x: CellProps[D, Option[Redshift], Plugins]) =>
              x.value.map(formatZ.reverseGet).orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("cz", (Target.Sidereal.radialVelocity.get _).andThen(rvToARVGet))
            .setCell((x: CellProps[D, Option[ApparentRadialVelocity], Plugins]) =>
              x.value.map(formatCZ.reverseGet).orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("parallax", Target.Sidereal.parallax.get)
            .setCell((x: CellProps[D, Option[Parallax], Plugins]) =>
              x.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty
            )
            .setSortByAuto,
          siderealColumn(
            "morphology",
            (Target.Sidereal.sourceProfile.get _).andThen(SourceProfileType.fromSourceProfile)
          )
            .setCell((x: CellProps[D, Option[SourceProfileType], Plugins]) =>
              x.value.map(_.shortName).orEmpty
            ),
          siderealColumn(
            "sed",
            t =>
              Target.Sidereal.integratedSpectralDefinition
                .getOption(t)
                .map(_.shortName)
                .orElse(Target.Sidereal.surfaceSpectralDefinition.getOption(t).map(_.shortName))
          )
            .setCell((x: CellProps[D, Option[Option[String]], Plugins]) => x.value.orEmpty)
        )
  }

  trait TargetColumnBuilder[D] {
    val getTarget: D => Option[Target]

    val getSiderealTarget: D => Option[Target.Sidereal] =
      getTarget.andThen(_.flatMap(_ match {
        case s @ Target.Sidereal(_, _, _, _) => Some(s)
        case _                               => None
      }))
  }

  case class BaseColumnBuilder[D, Plugins, Layout](tableDef: TableDef[D, Plugins, Layout])(
    val getTarget:                                           D => Option[Target]
  )(implicit val sortByEv:                                   Plugins <:< Plugin.SortBy.Tag)
      extends TargetColumnBuilder[D]
      with BaseColBuilder[D, Plugins, Layout]
      with SiderealColBuilder[D, Plugins, Layout] {
    // with NonsiderealColBuilder[D, Plugins, Layout] {
    lazy val allColumns = baseColumns ++ siderealColumns
  }

  case class NonBaseSiderealColumnBuilder[D, Plugins, Layout](
    tableDef:              TableDef[D, Plugins, Layout]
  )(
    val getTarget:         D => Option[Target]
  )(implicit val sortByEv: Plugins <:< Plugin.SortBy.Tag)
      extends TargetColumnBuilder[D]
      with SiderealColBuilder[D, Plugins, Layout] {
    // with NonsiderealColBuilder[D, Plugins, Layout] {
    lazy val allColumns = siderealColumns
  }
}
