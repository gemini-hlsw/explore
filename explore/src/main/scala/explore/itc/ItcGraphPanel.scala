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
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.UnderConstruction
import explore.common.ObsQueries.*
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
import explore.model.reusability.*
import explore.model.reusability.given
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import queries.schemas.itc.implicits.*
import react.common.ReactFnProps

import java.util.UUID

case class ItcGraphPanel(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedTarget:           View[Option[ItcTarget]]
)(using val ctx:            AppContextIO)
    extends ReactFnProps(ItcGraphPanel.component)
    with ItcPanelProps(scienceMode, spectroscopyRequirements, scienceData, exposure)

object ItcGraphPanel {
  private type Props = ItcGraphPanel with ItcPanelProps

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(Pot.pending[Map[ItcTarget, ItcChartResult]])
      // loading
      .useState(PlotLoading.Done)
      // Request ITC graph data
      .useEffectWithDepsBy((props, _, _) => props.queryProps) { (props, charts, loading) => _ =>
        import props.given
        props.requestITCData(
          m =>
            charts.modStateAsync {
              case Pot.Ready(r) => Pot.Ready(r + (m.target -> m))
              case u            => Pot.Ready(Map(m.target -> m))
            } *> loading.setState(PlotLoading.Done).to[IO],
          (charts.setState(
            Pot.error(new RuntimeException("Not enough information to call ITC"))
          ) *> loading.setState(PlotLoading.Done)).to[IO],
          loading.setState(PlotLoading.Loading).to[IO]
        )
      }
      // Default selected chart
      .useStateView(ItcChartType.S2NChart)
      // show description
      .useStateView(PlotDetails.Shown)
      .render { (props, results, loading, chartType, details) =>
        val error: Option[String] = results.value.fold(none, _.getMessage.some, _ => none)

        val selectedResult: Option[ItcChartResult] =
          props.selectedTarget.get.flatMap(t => results.value.toOption.flatMap(_.get(t)))

        <.div(
          ExploreStyles.ItcPlotSection,
          ExploreStyles.ItcPlotDetailsHidden.unless(details.when(_.value)),
          ItcSpectroscopyPlotDescription(props.chartExposureTime, selectedResult.map(_.ccds)),
          ItcSpectroscopyPlot(
            selectedResult.map(_.charts),
            error,
            chartType.get,
            props.selectedTarget.get.map(_.name.value),
            loading.value,
            details.get
          ),
          ItcPlotControl(chartType, details)
        )
      }
}
