// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order
import cats.Order.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators.*
import explore.model.conversions.*
import explore.model.display.*
import explore.model.display.given
import explore.model.enums.SourceProfileType
import explore.model.formats.*
import explore.optics.all.*
import explore.utils.given
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion.Dec
import lucuma.core.math.ProperMotion.RA
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Target
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.syntax.all.given
import org.scalablytyped.runtime.StringDictionary
import reactST.{tanstackTableCore => raw}

object TargetColumns:
  val TypeColumnId: ColumnId       = ColumnId("type")
  val NameColumnId: ColumnId       = ColumnId("name")
  val RAColumnId: ColumnId         = ColumnId("ra")
  val DecColumnId: ColumnId        = ColumnId("dec")
  val EpochColumnId: ColumnId      = ColumnId("epoch")
  val PMRAColumnId: ColumnId       = ColumnId("pmra")
  val PMDecColumnId: ColumnId      = ColumnId("pmdec")
  val RVColumnId: ColumnId         = ColumnId("rv")
  val ZColumnId: ColumnId          = ColumnId("z")
  val CZColumnId: ColumnId         = ColumnId("cz")
  val ParallaxColumnId: ColumnId   = ColumnId("parallax")
  val MorphologyColumnId: ColumnId = ColumnId("morphology")
  val SEDColumnId: ColumnId        = ColumnId("sed")

  def bandColumnId(band: Band): ColumnId = ColumnId(s"${band.tag}mag")

  val baseColNames: Map[ColumnId, String] = Map(
    TypeColumnId -> " ",
    NameColumnId -> "Name"
  )

  val siderealColNames: Map[ColumnId, String] = Map(
    TypeColumnId       -> "Type",
    RAColumnId         -> "RA",
    DecColumnId        -> "Dec",
    EpochColumnId      -> "Epoch",
    PMRAColumnId       -> "µ RA",
    PMDecColumnId      -> "µ Dec",
    RVColumnId         -> "RV",
    ZColumnId          -> "z",
    CZColumnId         -> "cz",
    ParallaxColumnId   -> "Parallax",
    MorphologyColumnId -> "Morphology",
    SEDColumnId        -> "SED"
  ) ++ Band.all.map(b => bandColumnId(b) -> b.shortName).toMap

  val allColNames: Map[ColumnId, String] = baseColNames ++ siderealColNames

  val DefaultVisibility: ColumnVisibility =
    ColumnVisibility(
      (List(
        EpochColumnId,
        PMRAColumnId,
        PMDecColumnId,
        ZColumnId,
        CZColumnId,
        ParallaxColumnId,
        MorphologyColumnId,
        SEDColumnId
      ) ++
        Band.all
          .filterNot(_ === Band.V)
          .map(b => bandColumnId(b))).map(_ -> Visibility.Hidden): _*
    )

  trait BaseColBuilder[D](colDef: ColumnDef.Applied[D], getTarget: D => Option[Target]):
    def baseColumn[V](id: ColumnId, accessor: Target => V): ColumnDef.Single[D, Option[V]] =
      colDef(id, getTarget.andThen(_.map(accessor)), baseColNames(id))

    val baseColumns =
      List(
        baseColumn(TypeColumnId, _ => ())
          .setCell(_ => Icons.Star.withFixedWidth(): VdomNode)
          .setSize(35.toPx),
        baseColumn(NameColumnId, Target.name.get)
          .setCell(_.value.map(_.toString).orEmpty)
          .setSize(120.toPx)
          .sortableBy(_.toString)
      )

  trait SiderealColBuilder[D](
    colDef:            ColumnDef.Applied[D],
    getSiderealTarget: D => Option[Target.Sidereal]
  ):
    def siderealColumnOpt[V](
      id:       ColumnId,
      accessor: Target.Sidereal => Option[V]
    ): ColumnDef.Single[D, Option[V]] =
      colDef(id, getSiderealTarget.andThen(_.flatMap(accessor)), siderealColNames(id))

    def siderealColumn[V](
      id:       ColumnId,
      accessor: Target.Sidereal => V
    ): ColumnDef.Single[D, Option[V]] =
      siderealColumnOpt(id, accessor.andThen(_.some))

    /** Display measure without the uncertainty */
    def displayWithoutError[N: Display](measure: Measure[N]): VdomNode =
      <.div(
        <.span(measure.value.shortName),
        <.span(ExploreStyles.UnitsTableLabel, measure.units.shortName.replace(" mag", ""))
      )

    // Order first by unit alphabetically and then value
    private given Order[Measure[BrightnessValue]] = Order.by(x => (x.units.abbv, x.value))

    val siderealColumns =
      List(
        siderealColumn(RAColumnId, Target.Sidereal.baseRA.get)
          .setCell(_.value.map(MathValidators.truncatedRA.reverseGet).orEmpty)
          .setSize(100.toPx)
          .sortable,
        siderealColumn(DecColumnId, Target.Sidereal.baseDec.get)
          .setCell(_.value.map(MathValidators.truncatedDec.reverseGet).orEmpty)
          .setSize(100.toPx)
          .sortable
      ) ++
        Band.all.map(band =>
          siderealColumnOpt(
            bandColumnId(band),
            t => targetBrightnesses.get(t).flatMap(_.get(band))
          ).setCell(_.value.map(displayWithoutError).orEmpty)
            .setSize(80.toPx)
            // By user request we allow sorting by value though there maybe a mix of units
            .sortable
        ) ++
        List(
          siderealColumn(EpochColumnId, Target.Sidereal.epoch.get)
            .setCell(
              _.value
                .map(value =>
                  s"${value.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(value)}"
                )
                .orEmpty
            )
            .setSize(90.toPx)
            .sortable,
          siderealColumnOpt(PMRAColumnId, Target.Sidereal.properMotionRA.getOption)
            .setCell(_.value.map(pmRAValidWedge.reverseGet).orEmpty)
            .setSize(90.toPx)
            .sortable,
          siderealColumnOpt(PMDecColumnId, Target.Sidereal.properMotionDec.getOption)
            .setCell(_.value.map(pmDecValidWedge.reverseGet).orEmpty)
            .setSize(90.toPx)
            .sortable,
          siderealColumnOpt(RVColumnId, Target.Sidereal.radialVelocity.get)
            .setCell(_.value.map(formatRV.reverseGet).orEmpty)
            .setSize(90.toPx)
            .sortable,
          siderealColumnOpt(
            ZColumnId,
            (Target.Sidereal.radialVelocity.get _).andThen(rvToRedshiftGet)
          )
            .setCell(_.value.map(formatZ.reverseGet).orEmpty)
            .setSize(90.toPx)
            .sortable,
          siderealColumnOpt(CZColumnId, (Target.Sidereal.radialVelocity.get _).andThen(rvToARVGet))
            .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
            .setSize(90.toPx)
            .sortable,
          siderealColumnOpt(ParallaxColumnId, Target.Sidereal.parallax.get)
            .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
            .setSize(90.toPx)
            .sortable,
          siderealColumn(
            MorphologyColumnId,
            (Target.Sidereal.sourceProfile.get _).andThen(SourceProfileType.fromSourceProfile)
          )
            .setCell(_.value.map(_.shortName).orEmpty)
            .setSize(115.toPx)
            .sortable,
          siderealColumn(
            SEDColumnId,
            t =>
              Target.Sidereal.integratedSpectralDefinition
                .getOption(t)
                .map(_.shortName)
                .orElse(Target.Sidereal.surfaceSpectralDefinition.getOption(t).map(_.shortName))
                .orEmpty
          )
            .setCell(_.value.orEmpty)
            .setSize(200.toPx)
            .sortable
        )

  case class BaseColumnBuilder[D](colDef: ColumnDef.Applied[D], getTarget: D => Option[Target])
      extends BaseColBuilder(colDef, getTarget)
      with SiderealColBuilder(colDef, getTarget.andThen(_.flatMap(Target.sidereal.getOption))):
    // with NonsiderealColBuilder:
    lazy val allColumns = baseColumns ++ siderealColumns

  case class NonBaseSiderealColumnBuilder[D](
    colDef:    ColumnDef.Applied[D],
    getTarget: D => Option[Target]
  ) extends SiderealColBuilder(colDef, getTarget.andThen(_.flatMap(Target.sidereal.getOption))):
    // with NonsiderealColBuilder]:
    lazy val allColumns = siderealColumns
