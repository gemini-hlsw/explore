// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.given
import explore.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.sequence.byInstrument.*
import explore.model.AsterismIds
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given

object SequenceTile extends SequenceTileHelper:
  def apply(
    obsId:               Observation.Id,
    obsExecution:        Pot[Execution],
    asterismIds:         AsterismIds,
    customSedTimestamps: List[Timestamp],
    sequenceChanged:     View[Pot[Unit]]
  ) =
    Tile(
      ObsTabTileIds.SequenceId.id,
      "Sequence",
      initialState = false
    )(
      isRefreshing =>
        Body(
          obsId,
          asterismIds.toList,
          customSedTimestamps,
          sequenceChanged,
          isRefreshing.set
        ),
      (isRefreshing, _) => Title(obsExecution, isRefreshing.get)
    )

  private case class Body(
    obsId:               Observation.Id,
    targetIds:           List[Target.Id],
    customSedTimestamps: List[Timestamp],
    sequenceChanged:     View[Pot[Unit]],
    setIsRefreshing:     Boolean => Callback
  ) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>
        for
          liveSequence <- useLiveSequence(props.obsId, props.targetIds, props.customSedTimestamps)
          _            <- useEffectWithDeps(liveSequence.data): dataPot =>
                            props.sequenceChanged.set(dataPot.void)
          _            <- useEffectWithDeps(liveSequence.refreshing):
                            props.setIsRefreshing(_)
        yield props.sequenceChanged.get
          .flatMap(_ => liveSequence.data)
          .renderPot(
            (visitsOpt, sequenceDataOpt) =>
              // TODO Show visits even if sequence data is not available
              sequenceDataOpt
                .fold[VdomNode](<.div("Empty or incomplete sequence data returned by server")) {
                  case SequenceData(InstrumentExecutionConfig.GmosNorth(config), snPerClass) =>
                    GmosNorthSequenceTable(
                      visitsOpt
                        .collect:
                          case ExecutionVisits.GmosNorth(visits) => visits.toList
                        .orEmpty,
                      config,
                      snPerClass
                    )
                  case SequenceData(InstrumentExecutionConfig.GmosSouth(config), snPerClass) =>
                    GmosSouthSequenceTable(
                      visitsOpt
                        .collect:
                          case ExecutionVisits.GmosSouth(visits) => visits.toList
                        .orEmpty,
                      config,
                      snPerClass
                    )
                },
            errorRender = m =>
              <.div(ExploreStyles.SequencesPanelError)(
                Message(
                  text = m.getMessage,
                  severity = Message.Severity.Warning,
                  icon = Icons.ExclamationTriangle
                )
              )
          )
      )

  private case class Title(obsExecution: Pot[Execution], isRefreshing: Boolean)
      extends ReactFnProps(Title)

  private object Title
      extends ReactFnComponent[Title](props =>
        <.span(ExploreStyles.SequenceTileTitle)(
          props.obsExecution
            .filter(_ => !props.isRefreshing)
            .orSpinner: execution =>
              val programTimeCharge = execution.programTimeCharge.value

              val executed = timeDisplay("Executed", programTimeCharge)

              execution.programTimeEstimate
                .map: plannedTime =>
                  val total   = programTimeCharge +| plannedTime
                  val pending = timeDisplay("Pending", plannedTime)
                  val planned = timeDisplay("Planned", total)

                  React.Fragment(
                    HelpIcon("target/main/sequence-times.md".refined),
                    planned,
                    executed,
                    pending
                  )
                .getOrElse(executed)
        )
      )
