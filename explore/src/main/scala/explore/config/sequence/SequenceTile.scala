// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.View
import crystal.react.given
import crystal.react.hooks.*
import explore.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.AsterismIds
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObserveClass
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.odb.SequenceQueriesGQL.*
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL
import queries.common.VisitQueriesGQL.*

object SequenceTile:
  def apply(
    programId:       Program.Id,
    obsId:           Observation.Id,
    obsExecution:    Pot[Execution],
    asterismIds:     AsterismIds,
    sequenceChanged: View[Pot[Unit]]
  ) =
    Tile(
      ObsTabTileIds.SequenceId.id,
      s"Sequence"
    )(
      _ =>
        Body(
          programId,
          obsId,
          asterismIds.toList,
          sequenceChanged
        ),
      (_, _) => Title(obsExecution)
    )

  private case class Body(
    programId:       Program.Id,
    obsId:           Observation.Id,
    targetIds:       List[Target.Id],
    sequenceChanged: View[Pot[Unit]]
  ) extends ReactFnProps(Body.component)

  private object Body:
    private type Props = Body

    private case class SequenceData(
      config:     InstrumentExecutionConfig,
      snPerClass: Map[ObserveClass, SignalToNoise]
    ) derives Eq

    private object SequenceData:
      def fromOdbResponse(data: SequenceQuery.Data): Option[SequenceData] =
        data.observation.flatMap: obs =>
          obs.execution.config.map: config =>
            SequenceData(
              config,
              Map.empty
              // (
              //   ObserveClass.Science     -> obs.itc.science.selected.signalToNoise,
              //   ObserveClass.Acquisition -> obs.itc.acquisition.selected.signalToNoise
              // )
            )

      given Reusability[SequenceData] = Reusability.byEq

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .useContext(AppContext.ctx)
        .useStreamResourceOnMountBy: (props, ctx) => // Query Visits
          import ctx.given

          ObservationVisits[IO]
            .query(props.obsId)
            .map(_.flatMap(_.observation.flatMap(_.execution)))
            .attemptPot
            .resetOnResourceSignals:
              ObsQueriesGQL.ObservationEditSubscription
                .subscribe[IO]:
                  props.obsId.toObservationEditInput
        .useStreamResourceOnMountBy: (props, ctx, _) => // Query Sequence
          import ctx.given

          val targetChanges = props.targetIds.traverse: targetId =>
            TargetQueriesGQL.TargetEditSubscription
              .subscribe[IO](targetId)

          SequenceQuery[IO]
            .query(props.obsId)
            .map(SequenceData.fromOdbResponse)
            .attemptPot
            .resetOnResourceSignals:
              for
                o <- ObsQueriesGQL.ObservationEditSubscription
                       .subscribe[IO](props.obsId.toObservationEditInput)
                t <- targetChanges
              yield o.merge(t.sequence)
        .useEffectWithDepsBy((_, _, visits, sequenceData) =>
          (visits.toPot.flatten, sequenceData.toPot.flatten).tupled
        ): (props, _, _, _) =>
          dataPot => props.sequenceChanged.set(dataPot.void)
        .render: (props, _, visits, sequenceData) =>
          props.sequenceChanged.get
            .flatMap(_ =>
              (visits.toPot.flatten, sequenceData.toPot.flatten).tupled
            ) // tupled for Pot
            .renderPot(
              (visitsOpt, sequenceDataOpt) =>
                // TODO Show visits even if sequence data is not available
                sequenceDataOpt
                  .fold[VdomNode](<.div("Empty or incomplete sequence data returned by server")) {
                    case SequenceData(InstrumentExecutionConfig.GmosNorth(config), snPerClass) =>
                      GmosNorthSequenceTable(
                        visitsOpt.collect { case ExecutionVisits.GmosNorth(visits) =>
                          visits.toList
                        }.orEmpty,
                        config,
                        snPerClass
                      )
                    case SequenceData(InstrumentExecutionConfig.GmosSouth(config), snPerClass) =>
                      GmosSouthSequenceTable(
                        visitsOpt.collect { case ExecutionVisits.GmosSouth(visits) =>
                          visits.toList
                        }.orEmpty,
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

  private case class Title(obsExecution: Pot[Execution]) extends ReactFnProps(Title.component)

  private object Title:
    private type Props = Title

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .render: props =>
          <.span(ExploreStyles.SequenceTileTitle)(
            props.obsExecution.orSpinner: execution =>
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
