// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import cats.syntax.all.*
import clue.ErrorPolicy
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.syntax.all.given
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.AccordionMultiple
import lucuma.react.primereact.AccordionTab
import lucuma.schemas.model.AtomRecord
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.StepRecord
import lucuma.schemas.model.Visit
import lucuma.schemas.odb.input.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ObsQueriesGQL
import queries.common.VisitQueriesGQL.*

import java.time.Duration

case class VisitsViewer(obsId: Observation.Id) extends ReactFnProps(VisitsViewer.component)

object VisitsViewer:
  private type Props = VisitsViewer

  private def stepDuration[D](step: StepRecord[D]): Duration =
    step.instrumentConfig match
      case DynamicConfig.GmosNorth(exposure, _, _, _, _, _, _) => exposure.toDuration
      case DynamicConfig.GmosSouth(exposure, _, _, _, _, _, _) => exposure.toDuration

  private def renderSequence[D](
    sequenceType: SequenceType,
    atoms:        List[AtomRecord[D]],
    renderTable:  List[StepRecord[D]] => VdomNode
  ): Option[AccordionTab] =
    atoms
      .flatMap(_.steps)
      .some
      .filter(_.nonEmpty)
      .map: steps =>
        AccordionTab(
          clazz = ExploreStyles.VisitSection,
          header = <.div(ExploreStyles.VisitHeader)( // Steps is non-empty => head is safe
            <.span(Constants.UtcFormatter.format(steps.head.created.toInstant)),
            // <.span(Constants.UtcFormatter.format(steps.head.interval.start.toInstant)),
            <.span(sequenceType.shortName),
            <.span(s"Steps: 1 - ${steps.length}"),
            <.span {
              val datasetIndices = steps.flatMap(_.datasets).map(_.index.value)
              "Files: " + datasetIndices.minOption
                .map(min => s"$min - ${datasetIndices.max}")
                .getOrElse("---")
            },
            <.span(
              Constants.DurationFormatter(
                steps
                  .map(step => stepDuration(step))
                  .reduce(_.plus(_))
              )
            )
          )
        )(renderTable(steps))

  def renderVisits[D](
    visits:      List[Visit[D]],
    renderTable: List[StepRecord[D]] => VdomNode
  ): List[AccordionTab] =
    visits
      .flatMap: visit =>
        renderSequence(SequenceType.Acquisition, visit.acquisitionAtoms, renderTable) ++
          renderSequence(SequenceType.Science, visit.scienceAtoms, renderTable)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy: (props, ctx) =>
        import ctx.given

        ObservationVisits[IO]
          .query(props.obsId)(ErrorPolicy.IgnoreOnData)
          .map(_.map(_.observation.map(_.execution)))
          .attemptPot
          .resetOnResourceSignals:
            ObsQueriesGQL.ObservationEditSubscription
              .subscribe[IO]:
                props.obsId.toObservationEditInput
      .render: (_, _, execution) =>
        execution.toPot.flatten
          .renderPot:
            _.map:
              _.map: executionVisits =>
                val accordionTabs =
                  executionVisits match
                    case ExecutionVisits.GmosNorth(_, visits) =>
                      renderVisits(visits, GmosNorthVisitTable(_))
                    case ExecutionVisits.GmosSouth(_, visits) =>
                      renderVisits(visits, GmosSouthVisitTable(_))

                AccordionMultiple(tabs = accordionTabs)
