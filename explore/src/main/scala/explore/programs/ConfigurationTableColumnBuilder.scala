// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import explore.model.AppContext
import explore.model.Observation
import explore.model.TargetList
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Configuration
import lucuma.core.model.Configuration.ObservingMode.GmosNorthLongSlit
import lucuma.core.model.Configuration.ObservingMode.GmosSouthLongSlit
import lucuma.core.model.Program
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.react.table.ColumnDef
import lucuma.react.table.ColumnId
import lucuma.ui.syntax.table.*

import scala.collection.immutable.SortedSet

case class ConfigurationTableColumnBuilder[D, TM](colDef: ColumnDef.Applied[D, TM]):
  import ConfigurationTableColumnBuilder.*

  def configurationColumns(getConfiguration: D => Configuration) =
    def configurationColumn[V](
      id:       ColumnId,
      accessor: Configuration => V
    ) = colDef(id, r => accessor(getConfiguration(r)), ColumnNames(id))

    List(
      configurationColumn(RAColumnId, _.refererenceCoordinates.ra)
        .setCell(c => MathValidators.truncatedRA.reverseGet(c.value))
        .setSize(110.toPx)
        .sortable,
      configurationColumn(DecColumnId, _.refererenceCoordinates.dec)
        .setCell(c => MathValidators.truncatedDec.reverseGet(c.value))
        .setSize(110.toPx)
        .sortable,
      configurationColumn(InstrumentColumnId, _.observingMode.tpe.instrument.shortName)
        .setSize(110.toPx)
        .sortable,
      configurationColumn(FPUColumnId, _.observingMode.fpu).setSize(110.toPx).sortable,
      configurationColumn(DisperserColumnId, _.observingMode.disperser)
        .setSize(110.toPx)
        .sortable,
      configurationColumn(ImageQualityColumnId, _.conditions.imageQuality)
        .setCell(_.value.label)
        .setSize(80.toPx)
        .sortable,
      configurationColumn(CloudExtinctionColumnId, _.conditions.cloudExtinction)
        .setCell(_.value.label)
        .setSize(80.toPx)
        .sortable,
      configurationColumn(SkyBackgroundColumnId, _.conditions.skyBackground)
        .setCell(_.value.label)
        .setSize(80.toPx)
        .sortable,
      configurationColumn(WaterVaporColumnId, _.conditions.waterVapor)
        .setCell(_.value.label)
        .setSize(80.toPx)
        .sortable
    )

  def obsListColumn(
    accessor:  D => SortedSet[Observation.Id],
    programId: Program.Id,
    ctx:       AppContext[IO]
  ) =
    colDef(ObservationsColumnId, accessor, ColumnNames(ObservationsColumnId))
      .setCell(c =>
        <.span(
          c.value.toList
            .map(obsId => ctx.obsIdRoutingLink(programId, obsId))
            .mkReactFragment(", ")
        )
      )
      .setSize(150.toPx)
      .setEnableSorting(false)

  def targetColumn(accessor: D => String) =
    colDef(TargetColumnId, accessor, ColumnNames(TargetColumnId))
      .setSize(150.toPx)
      .sortable

object ConfigurationTableColumnBuilder {
  private val TargetColumnId          = ColumnId("target")
  private val RAColumnId              = ColumnId("ra")
  private val DecColumnId             = ColumnId("dec")
  private val InstrumentColumnId      = ColumnId("instrument")
  private val FPUColumnId             = ColumnId("fpu")
  private val DisperserColumnId       = ColumnId("disperser")
  private val ImageQualityColumnId    = ColumnId("image_quality")
  private val CloudExtinctionColumnId = ColumnId("cloud_extinction")
  private val SkyBackgroundColumnId   = ColumnId("sky_background")
  private val WaterVaporColumnId      = ColumnId("water_vapor")
  private val ObservationsColumnId    = ColumnId("observations")

  val ColumnNames: Map[ColumnId, String] = Map(
    TargetColumnId          -> "Target",
    RAColumnId              -> "RA",
    DecColumnId             -> "Dec",
    InstrumentColumnId      -> "Instrument",
    FPUColumnId             -> "FPU",
    DisperserColumnId       -> "Disperser",
    ImageQualityColumnId    -> "IQ",
    CloudExtinctionColumnId -> "CC",
    SkyBackgroundColumnId   -> "SB",
    WaterVaporColumnId      -> "WV",
    ObservationsColumnId    -> "Obs"
  )

  extension (mode: Configuration.ObservingMode)
    def fpu: String       = mode match
      case GmosNorthLongSlit(_) => "LongSlit"
      case GmosSouthLongSlit(_) => "LongSlit"
    def disperser: String = mode match
      case GmosNorthLongSlit(grating) => grating.shortName
      case GmosSouthLongSlit(grating) => grating.shortName

  def targetName(observations: List[Observation], targets: TargetList): String =
    val targetNames =
      observations
        .flatMap(_.scienceTargetIds)
        .distinct
        .map(tid => targets.get(tid).map(_.name.value))
        .flattenOption
        .distinct
    targetNames match
      case head :: Nil  => head
      case head :: next => "<multiple>"
      case Nil          => "<none>"

}
