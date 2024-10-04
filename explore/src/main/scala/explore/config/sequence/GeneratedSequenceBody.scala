// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import explore.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Execution
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
import crystal.syntax.*

case class GeneratedSequenceBody(
  programId:       Program.Id,
  obsId:           Observation.Id,
  targetIds:       List[Target.Id],
  sequenceChanged: View[Pot[Unit]]
) extends ReactFnProps(GeneratedSequenceBody.component)

object GeneratedSequenceBody:
  private type Props = GeneratedSequenceBody

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
            Map(
              ObserveClass.Science     -> obs.itc.science.selected.signalToNoise,
              ObserveClass.Acquisition -> obs.itc.acquisition.selected.signalToNoise
            )
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
          .adaptError:
            case t @ clue.ResponseException(errors, _) =>
              val x =
                errors
                  .collectFirstSome: error =>
                    for
                      extensions   <- error.extensions
                      odbErrorJson <- extensions.get("odb_error")
                      odbErrorJObj <- odbErrorJson.asObject
                      _             = println(s"3: $odbErrorJObj")
                      tagJson      <- odbErrorJObj("odb_error.tag")
                      _             = println(s"4: $tagJson")
                      tag          <- tagJson.asString if tag == "sequence_unavailable"
                      _             = println(s"5: $tag")
                    yield new RuntimeException(
                      "No sequence available, you may need to setup a configuration"
                    )
              println(x)
              x.getOrElse(t)
          .attemptPot
          .flatTap(IO.println)
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
        println(s"visits: ${visits.toPot.flatten}")
        println(s"sequenceData: ${sequenceData.toPot.flatten}")
        props.sequenceChanged.get
          .flatMap(_ => (visits.toPot.flatten, sequenceData.toPot.flatten).tupled) // tupled for Pot
          .renderPot(
            _.tupled // tupled for Option
              .fold[VdomNode](<.div("Empty or incomplete sequence data returned by server")) {
                case (
                      ExecutionVisits.GmosNorth(_, visits),
                      SequenceData(InstrumentExecutionConfig.GmosNorth(config), snPerClass)
                    ) =>
                  GmosNorthSequenceTable(visits, config, snPerClass)
                case (
                      ExecutionVisits.GmosSouth(_, visits),
                      SequenceData(InstrumentExecutionConfig.GmosSouth(config), snPerClass)
                    ) =>
                  GmosSouthSequenceTable(visits, config, snPerClass)
                case _ => // Should never happen
                  <.div("There was an internal incosistency. Observation data may be corrupted.")
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

case class GeneratedSequenceTitle(
  obsExecution: Pot[Execution]
) extends ReactFnProps(GeneratedSequenceTitle.component)

object GeneratedSequenceTitle:
  private type Props = GeneratedSequenceTitle

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .render: props =>
        props.obsExecution.orSpinner { execution =>
          val programTimeCharge = execution.programTimeCharge.value

          val executed = timeDisplay("Executed", programTimeCharge)

          execution.programTimeEstimate
            .map { plannedTime =>
              val total   = programTimeCharge +| plannedTime
              val pending = timeDisplay("Pending", plannedTime)
              val planned = timeDisplay("Planned", total)
              <.span(ExploreStyles.SequenceTileTitle)(
                HelpIcon("target/main/sequence-times.md".refined),
                planned,
                executed,
                pending
              )
            }
            .getOrElse(executed)
        }
