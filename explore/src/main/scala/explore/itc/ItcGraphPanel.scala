// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import eu.timepit.refined.*
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.LoadingState
import explore.model.itc.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.itc.ChartType
import lucuma.react.common.ReactFnProps
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import monocle.Focus
import monocle.Lens

case class ItcGraphPanel(
  uid:             User.Id,
  oid:             Observation.Id,
  selectedTarget:  View[Option[ItcTarget]],
  itcProps:        ItcProps,
  itcChartResults: Map[ItcTarget, Pot[ItcChartResult]],
  itcLoading:      LoadingState
) extends ReactFnProps(ItcGraphPanel.component)

case class ItcGraphProperties(chartType: ChartType, detailsShown: PlotDetails)
object ItcGraphProperties:
  val chartType: Lens[ItcGraphProperties, ChartType] =
    Focus[ItcGraphProperties](_.chartType)

  val detailsShown: Lens[ItcGraphProperties, PlotDetails] =
    Focus[ItcGraphProperties](_.detailsShown)

object ItcGraphPanel:
  private type Props = ItcGraphPanel

  private given Reusability[PlotDetails]        = Reusability.byEq
  private given Reusability[ItcGraphProperties] = Reusability.by(x => (x.chartType, x.detailsShown))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Graph properties
      .useStateView(Pot.pending[ItcGraphProperties])
      // Read preferences
      .useEffectWithDepsBy((props, _, _) => (props.uid, props.oid)) { (props, ctx, settings) => _ =>
        import ctx.given

        ItcPlotPreferences
          .queryWithDefault[IO](props.uid, props.oid)
          .flatMap { (plotType, showDetails) =>
            settings.set(ItcGraphProperties(plotType, showDetails).ready).toAsync
          }
          .runAsyncAndForget
      }
      // Write preferences
      .useEffectWithDepsBy((_, _, settings) => settings.get) { (props, ctx, _) => settings =>
        import ctx.given

        settings.toOption
          .map(settings =>
            ItcPlotPreferences
              .updatePlotPreferences[IO](props.uid,
                                         props.oid,
                                         settings.chartType,
                                         settings.detailsShown
              )
              .runAsyncAndForget
          )
          .getOrEmpty
      }
      .render { (props, _, settings) =>
        val chartTypeView =
          settings.zoom(Pot.readyPrism.andThen(ItcGraphProperties.chartType))

        val detailsView =
          settings.zoom(Pot.readyPrism.andThen(ItcGraphProperties.detailsShown))

        val selectedTarget = props.selectedTarget.get

        val renderPlot: ItcGraphProperties => VdomNode =
          (opt: ItcGraphProperties) =>
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
              ExploreStyles.ItcPlotDetailsHidden.unless(opt.detailsShown.value),
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
                opt.chartType,
                props.selectedTarget.get.map(_.name.value),
                props.itcProps.signalToNoiseAt,
                props.itcLoading,
                opt.detailsShown
              ),
              ItcPlotControl(chartTypeView, detailsView)
            )

        settings.renderPotView(renderPlot)
      }
