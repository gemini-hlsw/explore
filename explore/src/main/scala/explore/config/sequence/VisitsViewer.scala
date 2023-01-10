// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.display.given
import explore.model.reusability.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.DynamicConfig
import lucuma.core.syntax.all.given
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.StepRecord
import lucuma.schemas.model.Visit
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import queries.common.ObsQueriesGQL
import queries.common.VisitsSQL.*
import react.common.ReactFnProps
import react.primereact.AccordionMultiple
import react.primereact.AccordionTab

import java.time.Duration
import java.time.ZoneOffset

case class VisitsViewer(obsId: Observation.Id) extends ReactFnProps(VisitsViewer.component)

object VisitsViewer:
  private type Props = VisitsViewer

  private def stepDuration(step: StepRecord): Duration =
    step.instrumentConfig match
      case DynamicConfig.GmosNorth(exposure, _, _, _, _, _, _) => exposure
      case DynamicConfig.GmosSouth(exposure, _, _, _, _, _, _) => exposure

  private def renderSequence(
    sequenceType: SequenceType,
    steps:        List[StepRecord]
  ): Option[AccordionTab] =
    Option.when(steps.nonEmpty)(
      AccordionTab(
        clazz = ExploreStyles.VisitSection,
        header = <.div(ExploreStyles.VisitHeader)(
          <.span(
            steps.headOption
              .flatMap(_.startTime)
              .fold("---")(start => Constants.UtcFormatter.format(start))
          ),
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
      )(VisitTable(steps))
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamResourceOnMountBy { (props, ctx) =>
        import ctx.given

        Visits
          .query(props.obsId)
          .map(_.observation.map(_.execution.executionConfig))
          .attemptPot
          .resetOnResourceSignals(
            ObsQueriesGQL.ObservationEditSubscription.subscribe[IO](props.obsId)
          )
      }
      .render((_, _, visits) =>
        visits.toPot.flatten
          .map(_.map(_.visits).orEmpty)
          .render(visits =>
            AccordionMultiple(tabs =
              visits
                .flatMap(visit =>
                  List(
                    renderSequence(SequenceType.Acquisition, visit.acquisitionSteps),
                    renderSequence(SequenceType.Science, visit.scienceSteps)
                  )
                )
                .flattenOption
            )
          )
      )
