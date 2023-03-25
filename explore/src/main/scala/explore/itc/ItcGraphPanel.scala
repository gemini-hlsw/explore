// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.IO
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import crystal.Pot
import crystal.implicits.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.UnderConstruction
import explore.common.UserPreferencesQueries.*
import explore.components.WIP
import explore.components.ui.ExploreStyles
import explore.config.ExposureTimeModeType.FixedExposure
import explore.events.*
import explore.model.AppContext
import explore.model.BasicConfigAndItc
import explore.model.LoadingState
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.enums.ItcChartType
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcSeries
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.model.itc.PlotDetails
import explore.model.reusability.given
import explore.model.reusability.given
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.schemas.model.ObservingMode
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import monocle.Focus
import monocle.Lens
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.itc.ITCConversions.*
import queries.schemas.odb.ObsQueries.*
import react.common.ReactFnProps

import java.util.UUID

case class ItcGraphPanel(
  uid:                      User.Id,
  oid:                      Observation.Id,
  observingMode:            Option[ObservingMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedTarget:           View[Option[ItcTarget]],
  selectedConfig:           Option[BasicConfigAndItc] // selected row in spectroscopy modes table
) extends ReactFnProps(ItcGraphPanel.component)
    with ItcPanelProps(observingMode,
                       spectroscopyRequirements,
                       scienceData,
                       exposure,
                       selectedConfig
    )

case class ItcGraphProperties(chartType: ItcChartType, detailsShown: PlotDetails)
object ItcGraphProperties:
  val chartType: Lens[ItcGraphProperties, ItcChartType] =
    Focus[ItcGraphProperties](_.chartType)

  val detailsShown: Lens[ItcGraphProperties, PlotDetails] =
    Focus[ItcGraphProperties](_.detailsShown)

object ItcGraphPanel:
  private type Props = ItcGraphPanel with ItcPanelProps

  private given Reusability[PlotDetails]        = Reusability.byEq
  private given Reusability[ItcGraphProperties] = Reusability.by(x => (x.chartType, x.detailsShown))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(Pot.pending[Map[ItcTarget, ItcChartResult]])
      // loading
      .useState(LoadingState.Done)
      // Request ITC graph data
      .useEffectWithDepsBy((props, _, _, _) => props.queryProps) {
        (props, ctx, charts, loading) => _ =>
          import ctx.given

          props
            .requestITCData(
              m =>
                charts.modStateAsync {
                  case Pot.Ready(r) => Pot.Ready(r + (m.target -> m))
                  case u            => Pot.Ready(Map(m.target -> m))
                } *> loading.setState(LoadingState.Done).to[IO],
              (charts.setState(
                Pot.error(new RuntimeException("Not enough information to calculate the ITC graph"))
              ) *> loading.setState(LoadingState.Done)).to[IO],
              loading.setState(LoadingState.Loading).to[IO]
            )
            .runAsyncAndForget
      }
      // Graph properties
      .useStateView(Pot.pending[ItcGraphProperties])
      // Read preferences
      .useEffectWithDepsBy((props, _, _, _, _) => (props.uid, props.oid)) {
        (props, ctx, _, _, settings) => _ =>
          import ctx.given

          ItcPlotPreferences
            .queryWithDefault[IO](props.uid, props.oid)
            .flatMap { (plotType, showDetails) =>
              settings.set(ItcGraphProperties(plotType, showDetails).ready).to[IO]
            }
            .runAsyncAndForget
      }
      // Write preferences
      .useEffectWithDepsBy((_, _, _, _, settings) => settings.get) {
        (props, ctx, _, _, _) => settings =>
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
      .render { (props, _, results, loading, settings) =>
        val chartTypeView =
          settings.zoom(Pot.readyPrism.andThen(ItcGraphProperties.chartType))

        val detailsView =
          settings.zoom(Pot.readyPrism.andThen(ItcGraphProperties.detailsShown))

        val renderPlot: ItcGraphProperties => VdomNode =
          (opt: ItcGraphProperties) =>
            val error: Option[String] = results.value.fold(none, _.getMessage.some, _ => none)

            val selectedResult: Option[ItcChartResult] =
              props.selectedTarget.get.flatMap(t => results.value.toOption.flatMap(_.get(t)))

            <.div(
              ExploreStyles.ItcPlotSection,
              ExploreStyles.ItcPlotDetailsHidden.unless(opt.detailsShown.value),
              ItcSpectroscopyPlotDescription(props.chartExposureTime, selectedResult.map(_.ccds)),
              ItcSpectroscopyPlot(
                selectedResult.map(_.ccds),
                selectedResult.map(_.charts),
                error,
                opt.chartType,
                props.selectedTarget.get.map(_.name.value),
                props.signalToNoiseAt,
                loading.value,
                opt.detailsShown
              ),
              ItcPlotControl(chartTypeView, detailsView)
            )

        settings.renderPotView(renderPlot)
      }
