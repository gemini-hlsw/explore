// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import eu.timepit.refined.*
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.GlobalPreferences
import explore.model.LoadingState
import explore.model.Observation
import explore.model.itc.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given
import monocle.Lens
import monocle.Focus

case class ItcPanelTileState(
  selectedTarget: Option[ItcTarget] = None
)

object ItcPanelTileState:
  val selectedTarget: Lens[ItcPanelTileState, Option[ItcTarget]] =
    Focus[ItcPanelTileState](_.selectedTarget)

case class ItcPanelBody(
  uid:               User.Id,
  oid:               Observation.Id,
  itcProps:          ItcProps,
  itcChartResults:   Map[ItcTarget, Pot[ItcChartResult]],
  itcLoading:        LoadingState,
  globalPreferences: View[GlobalPreferences],
  state:             View[ItcPanelTileState]
) extends ReactFnProps(ItcPanelBody.component) {
  val selectedTarget = state.zoom(ItcPanelTileState.selectedTarget)
}

object ItcPanelBody:
  private type Props = ItcPanelBody

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Reset the selected target if itcProps changes
      .useEffectWithDepsBy((p, _) => p.itcProps): (p, _) =>
        itcProps => p.selectedTarget.set(itcProps.defaultSelectedTarget)
      .render: (props, ctx) =>
        import ctx.given

        val globalPreferences = props.globalPreferences.withOnMod(prefs =>
          ItcPlotPreferences
            .updatePlotPreferences[IO](props.uid, prefs.itcChartType, prefs.itcDetailsOpen)
            .runAsyncAndForget
        )

        val chartTypeView =
          globalPreferences.zoom(GlobalPreferences.itcChartType)

        val detailsView =
          globalPreferences.zoom(GlobalPreferences.itcDetailsOpen)

        val selectedTarget = props.state.get.selectedTarget

        val isModeSelected = props.itcProps.finalConfig.isDefined
        val selectMode     = "Select a mode to plot".some.filterNot(_ => isModeSelected)

        val error: Option[String] =
          selectedTarget
            .fold("No target available".some)(t =>
              props.itcChartResults
                .get(t)
                .flatMap { r =>
                  r.fold(selectMode, _.getMessage.some, _ => none)
                }
                .orElse(selectMode)
            )

        val selectedResult: Option[ItcChartResult] =
          for {
            t <- selectedTarget
            r <- props.itcChartResults.get(t)
            c <- r.toOption
          } yield c

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
            selectedResult.map(_.charts),
            error,
            chartTypeView.get,
            props.state.get.selectedTarget.map(_.name.value),
            props.itcProps.signalToNoiseAt,
            props.itcLoading,
            detailsView.get
          ),
          ItcPlotControl(chartTypeView, detailsView)
        )
