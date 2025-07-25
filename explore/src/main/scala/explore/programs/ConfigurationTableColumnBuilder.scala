// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.syntax.display.*
import lucuma.react.syntax.*
import lucuma.react.table.*

case class ConfigurationTableColumnBuilder[D, TM, CM, TF](colDef: ColumnDef.Applied[D, TM, CM, TF]):
  import ConfigurationTableColumnBuilder.*

  def configurationColumns(getConfiguration: D => Configuration) =
    def configurationColumn[V](
      id:       ColumnId,
      accessor: Configuration => V
    ) = colDef(id, r => accessor(getConfiguration(r)), ColumnNames(id))

    List(
      configurationColumn(RAColumnId, _.refererenceCoordinates.ra)
        .withCell(c => MathValidators.truncatedRA.reverseGet(c.value))
        .withSize(110.toPx)
        .sortable,
      configurationColumn(DecColumnId, _.refererenceCoordinates.dec)
        .withCell(c => MathValidators.truncatedDec.reverseGet(c.value))
        .withSize(110.toPx)
        .sortable,
      configurationColumn(InstrumentColumnId, _.observingMode.tpe.instrument.shortName)
        .withSize(110.toPx)
        .sortable,
      configurationColumn(FPUColumnId, _.observingMode.fpu).withSize(110.toPx).sortable,
      configurationColumn(DisperserColumnId, _.observingMode.disperser)
        .withSize(110.toPx)
        .sortable,
      configurationColumn(ImageQualityColumnId, _.conditions.imageQuality)
        .withCell(_.value.shortName)
        .withSize(80.toPx)
        .sortable,
      configurationColumn(CloudExtinctionColumnId, _.conditions.cloudExtinction)
        .withCell(_.value.shortName)
        .withSize(80.toPx)
        .sortable,
      configurationColumn(SkyBackgroundColumnId, _.conditions.skyBackground)
        .withCell(_.value.label)
        .withSize(80.toPx)
        .sortable,
      configurationColumn(WaterVaporColumnId, _.conditions.waterVapor)
        .withCell(_.value.label)
        .withSize(80.toPx)
        .sortable
    )

  def obsListColumn(
    accessor:  D => List[Observation],
    programId: Program.Id,
    ctx:       AppContext[IO]
  ) =
    colDef(ObservationsColumnId, accessor, ColumnNames(ObservationsColumnId))
      .withCell(c =>
        <.span(
          c.value
            .sortBy(_.id)
            .map(obs =>
              if (obs.isInactive)
                ctx.obsIdRoutingLink(programId, obs.id, contents = <.s(obs.id.show).some)
              else ctx.obsIdRoutingLink(programId, obs.id)
            )
            .mkReactFragment(", ")
        )
      )
      .withSize(150.toPx)
      .withEnableSorting(false)

  def targetColumn(accessor: D => String) =
    colDef(TargetColumnId, accessor, ColumnNames(TargetColumnId))
      .withSize(150.toPx)
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
