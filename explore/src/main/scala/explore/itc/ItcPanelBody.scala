// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.*
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.GlobalPreferences
import explore.model.Observation
import explore.model.itc.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.itc.GraphType
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*

object SelectedItcTarget extends NewType[Option[ItcTarget]]:
  def apply(): SelectedItcTarget = SelectedItcTarget(None)
type SelectedItcTarget = SelectedItcTarget.Type

case class ItcPanelBody(
  uid:               User.Id,
  oid:               Observation.Id,
  itcProps:          ItcProps,
  itcGraphResults:   Pot[ItcAsterismGraphResults],
  globalPreferences: View[GlobalPreferences],
  tileState:         View[SelectedItcTarget]
) extends ReactFnProps(ItcPanelBody.component) {
  val selectedTarget = tileState.zoom(SelectedItcTarget.value.asLens)
}

object ItcPanelBody:
  private type Props = ItcPanelBody

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Reset the selected target if it changes
      .useEffectWhenDepsReadyBy((props, _) => props.itcGraphResults.map(_.brightestTarget)):
        (props, _) => itcBrightestTarget => props.selectedTarget.set(itcBrightestTarget)
      .render: (props, ctx) =>
        import ctx.given

        props.itcGraphResults.renderPot: graphResults =>
          val globalPreferences: View[GlobalPreferences] =
            props.globalPreferences.withOnMod: prefs =>
              ItcPlotPreferences
                .updatePlotPreferences[IO](props.uid, prefs.itcChartType, prefs.itcDetailsOpen)
                .runAsyncAndForget

          val graphTypeView: View[GraphType] =
            globalPreferences.zoom(GlobalPreferences.itcChartType)

          val detailsView: View[PlotDetails] =
            globalPreferences.zoom(GlobalPreferences.itcDetailsOpen)

          val selectedTarget: Option[ItcTarget] = props.selectedTarget.get

          val selectedResult: Option[ItcGraphResult] =
            for
              t <- selectedTarget
              r <- graphResults.asterismGraphs.get(t)
              c <- r.toOption
            yield c

          val isModeSelected: Boolean        =
            props.itcProps.selectedConfig.isDefined || selectedResult.isDefined
          val selectModeText: Option[String] =
            "Select a mode to plot".some.filterNot(_ => isModeSelected)

          val error: Option[String] =
            selectedTarget
              .fold("No target available".some): t =>
                graphResults.asterismGraphs
                  .get(t)
                  .flatMap:
                    _.left.toOption.map(_.message)
              .orElse(selectModeText)

          <.div(
            ExploreStyles.ItcPlotSection,
            ExploreStyles.ItcPlotDetailsHidden.unless(detailsView.get.value)
          )(
            ItcSpectroscopyPlotDescription(
              selectedTarget.flatMap(props.itcProps.targetBrightness),
              selectedResult.map(_.itcExposureTime),
              selectedResult.map(_.ccds),
              selectedResult.map(_.finalSNRatio),
              selectedResult.map(_.singleSNRatio)
            ),
            ItcSpectroscopyPlot(
              selectedResult.map(_.ccds),
              selectedResult.map(_.graphData),
              error,
              graphTypeView.get,
              selectedTarget.map(_.name.value),
              props.itcProps.signalToNoiseAt,
              detailsView.get
            ),
            ItcPlotControl(graphTypeView, detailsView)
          )
