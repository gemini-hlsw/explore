// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order
import cats.Order.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators.*
import explore.model.conversions.*
import explore.model.display.given
import explore.model.enums.SourceProfileType
import explore.model.formats.*
import explore.optics.all.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.CatalogInfo
import lucuma.core.model.Target
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.react.given
import monocle.std.option.*

import scala.collection.immutable.TreeSeqMap

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
  val CatalogName: ColumnId        = ColumnId("catalogName")
  val CatalogId: ColumnId          = ColumnId("catalogId")
  val CatalogObjectType: ColumnId  = ColumnId("catalogObjectType")

  def bandColumnId(band: Band): ColumnId = ColumnId(s"${band.tag}mag")

  val BaseColNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
      TypeColumnId      -> " ",
      NameColumnId      -> "Name",
      CatalogName       -> "Catalog",
      CatalogId         -> "Catalog Id",
      CatalogObjectType -> "Catalog Type"
    )

  val SiderealColNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
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

  val AllColNames: TreeSeqMap[ColumnId, String] = BaseColNames ++ SiderealColNames

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
          .map(b => bandColumnId(b))).map(_ -> Visibility.Hidden)*
    )

  object Builder:

    // type Type       = ColumnDef[T, ?, TM, CM, TF, ?, ?]
    // type TypeFor[A] = Single[T, A, TM, CM, TF, ?, ?]

    trait Common[D, TM, CM, TF](
      colDef:    ColumnDef.Applied[D, TM, CM, TF],
      getTarget: D => Option[Target]
    ):
      def baseColumn[V](id: ColumnId, accessor: Target => V): colDef.TypeFor[Option[V]] =
        colDef(id, getTarget.andThen(_.map(accessor)), BaseColNames(id))

      val NameColumn: colDef.TypeFor[Option[NonEmptyString]] =
        baseColumn(NameColumnId, Target.name.get)
          .setCell(_.value.map(_.toString).orEmpty)
          .setSize(120.toPx)
          .sortable

      val CatalogColumns: List[colDef.Type] =
        List(
          baseColumn(
            CatalogId,
            Target.catalogInfo.andThen(some).getOption(_).map(info => (info.id, info.objectUrl))
          ).setCell(
            _.value.flatten
              .map((id, uriOpt) =>
                uriOpt.fold[VdomNode](id.value)(uri =>
                  <.a(^.href := uri.toString, ^.target.blank)(id.value)
                )
              )
              .orEmpty
          ).setSize(100.toPx)
            .sortableBy(_.flatten.map(_._1).toString),
          baseColumn(
            CatalogObjectType,
            Target.catalogInfo
              .andThen(some)
              .andThen(CatalogInfo.objectType)
              .getOption
              .andThen(_.flatten)
          )
            .setCell(_.value.flatten.map(_.value).orEmpty)
            .setSize(100.toPx)
            .sortableBy(_.flatten.map(_.toString))
        )

    trait CommonSidereal[D, TM, CM, TF](
      colDef:            ColumnDef.Applied[D, TM, CM, TF],
      getSiderealTarget: D => Option[Target.Sidereal]
    ):
      def siderealColumnOpt[V](
        id:       ColumnId,
        accessor: Target.Sidereal => Option[V]
      ): colDef.TypeFor[Option[V]] =
        colDef(id, getSiderealTarget.andThen(_.flatMap(accessor)), SiderealColNames(id))

      def siderealColumn[V](
        id:       ColumnId,
        accessor: Target.Sidereal => V
      ): colDef.TypeFor[Option[V]] =
        siderealColumnOpt(id, accessor.andThen(_.some))

      /** Display measure without the uncertainty */
      def displayWithoutError[N: Display](measure: Measure[N]): VdomNode =
        <.div(
          <.span(measure.value.shortName),
          <.span(ExploreStyles.UnitsTableLabel, measure.units.shortName.replace(" mag", ""))
        )

      // Order first by unit alphabetically and then value
      private given Order[Measure[BrightnessValue]] = Order.by(x => (x.units.abbv, x.value))

      val SiderealColumns: List[colDef.Type] =
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
              t => BandNormalizedTargetBrightnesses.get(t).flatMap(_.get(band))
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
            siderealColumnOpt(ParallaxColumnId, Target.Sidereal.parallax.get)
              .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
              .setSize(90.toPx)
              .sortable,
            siderealColumnOpt(RVColumnId, Target.Sidereal.radialVelocity.get)
              .setCell(_.value.map(formatRV.reverseGet).orEmpty)
              .setSize(90.toPx)
              .sortable,
            siderealColumnOpt(
              ZColumnId,
              Target.Sidereal.radialVelocity.get.andThen(rvToRedshiftGet)
            )
              .setCell(_.value.map(formatZ.reverseGet).orEmpty)
              .setSize(90.toPx)
              .sortable
          )

    case class ForProgram[D, TM, CM, TF](
      colDef:    ColumnDef.Applied[D, TM, CM, TF],
      getTarget: D => Option[Target]
    ) extends Common(colDef, getTarget)
        with CommonSidereal(
          colDef,
          getTarget.andThen(_.flatMap(Target.sidereal.getOption))
        ):
      val BaseColumns: List[colDef.Type] =
        List(
          baseColumn(TypeColumnId, _ => ())
            .setCell(_ => Icons.Star.withFixedWidth(): VdomNode)
            .setSize(35.toPx),
          NameColumn,
          baseColumn(
            CatalogName,
            Target.catalogInfo.andThen(some).andThen(CatalogInfo.catalog).getOption
          )
            .setCell(_.value.flatten.map(_.shortName).orEmpty)
            .setSize(100.toPx)
            .sortableBy(_.flatten.map(_.toString))
        )

      val ProgramColumns: List[colDef.Type] = List(
        siderealColumnOpt(CZColumnId, Target.Sidereal.radialVelocity.get.andThen(rvToARVGet))
          .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
          .setSize(90.toPx)
          .sortable,
        siderealColumn(
          MorphologyColumnId,
          Target.Sidereal.sourceProfile.get.andThen(SourceProfileType.fromSourceProfile)
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

      lazy val AllColumns: List[colDef.Type] =
        BaseColumns ++ CatalogColumns ++ SiderealColumns ++ ProgramColumns

    case class ForSimbad[D, TM, CM, TF](
      colDef:    ColumnDef.Applied[D, TM, CM, TF],
      getTarget: D => Option[Target]
    ) extends Common(colDef, getTarget)
        with CommonSidereal(colDef, getTarget.andThen(_.flatMap(Target.sidereal.getOption))):
      lazy val AllColumns: List[colDef.Type] = (NameColumn +: CatalogColumns) ++ SiderealColumns
