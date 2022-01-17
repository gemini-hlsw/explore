// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.syntax.all._
import explore.Icons
import explore.model.conversions._
import explore.model.formats._
import explore.optics.ModelOptics._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Band
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.model.Target
import lucuma.ui.optics._
import reactST.reactTable.Plugin
import reactST.reactTable.TableDef

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
  ) ++ Band.all.map(m => (m.shortName + "mag", m.shortName + "Mag")).toMap

  val allColNames: Map[String, String] = baseColNames ++ siderealColNames

  // TODO In Scala 3, we should use trait parameters.

  trait BaseColBuilder[D, Plugins, Layout] {
    val tableDef: TableDef[D, Plugins, Layout]
    val getTarget: D => Option[Target]
    implicit val sortByEv: Plugins <:< Plugin.SortBy.Tag

    def baseColumn[V](id: String, accessor: Target => V) =
      tableDef
        .Column(id, getTarget.andThen(_.map(accessor)))
        .setHeader(baseColNames(id))

    val baseColumns =
      List(
        baseColumn("type", _ => ())
          .setCell(_ => Icons.Star)
          .setWidth(30),
        baseColumn("name", Target.name.get)
          .setCell(_.value.map(_.toString).orEmpty)
          .setSortByFn(_.toString)
      )
  }

  trait SiderealColBuilder[D, Plugins, Layout] {
    val tableDef: TableDef[D, Plugins, Layout]
    val getSiderealTarget: D => Option[Target.Sidereal]
    implicit val sortByEv: Plugins <:< Plugin.SortBy.Tag

    def siderealColumnOpt[V](id: String, accessor: Target.Sidereal => Option[V]) =
      tableDef
        .Column(id, getSiderealTarget.andThen(_.flatMap(accessor)))
        .setHeader(siderealColNames(id))

    def siderealColumn[V](id: String, accessor: Target.Sidereal => V) =
      siderealColumnOpt(id, accessor.andThen(_.some))

    val siderealColumns =
      List(
        siderealColumn("ra", Target.Sidereal.baseRA.get)
          .setCell(
            _.value
              .map(TruncatedRA.rightAscension.get.andThen(ValidFormatInput.truncatedRA.reverseGet))
              .orEmpty
          )
          .setSortByAuto,
        siderealColumn("dec", Target.Sidereal.baseDec.get)
          .setCell(
            _.value
              .map(TruncatedDec.declination.get.andThen(ValidFormatInput.truncatedDec.reverseGet))
              .orEmpty
          )
          .setSortByAuto
        // siderealColumn("priority", _ => "").setCell(_ =>
        //   ""
        // ) // TODO IS this a property of the target, or a property of the target in the observation???
      ) ++
        Band.all.map(band =>
          siderealColumnOpt(
            band.shortName + "mag",
            t => targetBrightnesses.get(t).flatMap(_.get(band))
          )
            .setCell(_.value.map(_.displayWithoutError).orEmpty)
            .setDisableSortBy(true) // We cannot sort since there may be different units.
        ) ++
        List(
          siderealColumn("epoch", Target.Sidereal.epoch.get)
            .setCell(
              _.value
                .map(value =>
                  s"${value.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(value)}"
                )
                .orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("pmra", Target.Sidereal.properMotionRA.getOption)
            .setCell(
              _.value.map(pmRAFormat.reverseGet).orEmpty
            )
            .setSortByAuto,
          siderealColumnOpt("pmdec", Target.Sidereal.properMotionDec.getOption)
            .setCell(_.value.map(pmDecFormat.reverseGet).orEmpty)
            .setSortByAuto,
          siderealColumnOpt("rv", Target.Sidereal.radialVelocity.get)
            .setCell(_.value.map(formatRV.reverseGet).orEmpty)
            .setSortByAuto,
          siderealColumnOpt("z", (Target.Sidereal.radialVelocity.get _).andThen(rvToRedshiftGet))
            .setCell(_.value.map(formatZ.reverseGet).orEmpty)
            .setSortByAuto,
          siderealColumnOpt("cz", (Target.Sidereal.radialVelocity.get _).andThen(rvToARVGet))
            .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
            .setSortByAuto,
          siderealColumnOpt("parallax", Target.Sidereal.parallax.get)
            .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
            .setSortByAuto,
          siderealColumn("morphology", _ => "").setCell(_ => ""),
          siderealColumn("sed", _ => "").setCell(_ => "")
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
