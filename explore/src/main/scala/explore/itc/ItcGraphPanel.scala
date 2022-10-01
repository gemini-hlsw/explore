// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.common.ObsQueries.*
import explore.common.UserPreferencesQueries.*
import explore.components.WIP
import explore.components.ui.ExploreStyles
import explore.config.ExposureTimeModeType.FixedExposure
import explore.events._
import explore.implicits.*
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
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
import explore.model.reusability.*
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
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens
import queries.common.UserPreferencesQueriesGQL._
import queries.schemas.itc.implicits.*
import react.common.ReactFnProps

import java.util.UUID

case class ItcGraphPanel(
  uid:                      User.Id,
  oid:                      Observation.Id,
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedTarget:           View[Option[ItcTarget]]
)(using val ctx:            AppContextIO)
    extends ReactFnProps(ItcGraphPanel.component)
    with ItcPanelProps(scienceMode, spectroscopyRequirements, scienceData, exposure)

case class ItcGraphProperties(chartType: ItcChartType, detailsShown: PlotDetails)
object ItcGraphProperties:
  val chartType: Lens[ItcGraphProperties, ItcChartType] =
    Focus[ItcGraphProperties](_.chartType)

  val detailsShown: Lens[ItcGraphProperties, PlotDetails] =
    Focus[ItcGraphProperties](_.detailsShown)

object ItcGraphPanel {
  private type Props = ItcGraphPanel with ItcPanelProps

  given Reusability[PlotDetails]        = Reusability.byEq
  given Reusability[ItcGraphProperties] = Reusability.by(x => (x.chartType, x.detailsShown))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(Pot.pending[Map[ItcTarget, ItcChartResult]])
      // loading
      .useState(PlotLoading.Done)
      // Request ITC graph data
      .useEffectWithDepsBy((props, _, _) => props.queryProps) { (props, charts, loading) => _ =>
        import props.given
        props
          .requestITCData(
            m =>
              charts.modStateAsync {
                case Pot.Ready(r) => Pot.Ready(r + (m.target -> m))
                case u            => Pot.Ready(Map(m.target -> m))
              } *> loading.setState(PlotLoading.Done).to[IO],
            (charts.setState(
              Pot.error(new RuntimeException("Not enough information to calculate the ITC graph"))
            ) *> loading.setState(PlotLoading.Done)).to[IO],
            loading.setState(PlotLoading.Loading).to[IO]
          )
          .runAsyncAndForget
      }
      // Graph properties
      .useStateView(Pot.pending[ItcGraphProperties])
      // Read preferences
      .useEffectWithDepsBy((props, _, _, _) => (props.uid, props.oid)) {
        (props, _, _, settings) => _ =>
          import props.given

          ItcPlotPreferences
            .queryWithDefault[IO](props.uid, props.oid)
            .flatMap { (plotType, showDetails) =>
              settings.set(ItcGraphProperties(plotType, showDetails).ready).to[IO]
            }
            .runAsyncAndForget
      }
      // Write preferences
      .useEffectWithDepsBy((_, _, _, settings) => settings.get) { (props, _, _, _) => settings =>
        import props.given

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
      .render { (props, results, loading, settings) =>
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

        potRenderView[ItcGraphProperties](renderPlot)(settings)
      }
}
