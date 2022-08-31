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

final case class ItcGraphPanel(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime]
)(using val ctx:            AppContextIO)
    extends ReactFnProps(ItcGraphPanel.component) {
  def wavelength: Option[Wavelength] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(_, adv)) =>
      adv.overrideWavelength.orElse(spectroscopyRequirements.flatMap(_.wavelength))

    case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
      adv.overrideWavelength.orElse(spectroscopyRequirements.flatMap(_.wavelength))

    case _ => none

  def signalToNoise: Option[PosBigDecimal] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(_, adv)) =>
      ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.signalToNoiseValue
        )
        .getOption(adv)
        .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

    case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
      ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.signalToNoiseValue
        )
        .getOption(adv)
        .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))

    case _ =>
      spectroscopyRequirements.flatMap(_.signalToNoise)

  def instrumentRow: Option[InstrumentRow] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(basic, adv)) =>
      val grating = adv.overrideGrating.getOrElse(basic.grating)
      val filter  = adv.overrideFilter.orElse(basic.filter)
      val fpu     = adv.overrideFpu.getOrElse(basic.fpu)
      GmosNorthSpectroscopyRow(grating, fpu, filter).some

    case Some(ScienceMode.GmosSouthLongSlit(basic, adv)) =>
      val grating = adv.overrideGrating.getOrElse(basic.grating)
      val filter  = adv.overrideFilter.orElse(basic.filter)
      val fpu     = adv.overrideFpu.getOrElse(basic.fpu)
      GmosSouthSpectroscopyRow(grating, fpu, filter).some

    case _ =>
      none

  def chartExposureTime: Option[ItcChartExposureTime] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(basic, adv)) =>
      ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.fixedExposure
        )
        .getOption(adv)
        .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
        .orElse(
          exposure.map(r => ItcChartExposureTime(OverridenExposureTime.Overriden, r.time, r.count))
        )

    case Some(ScienceMode.GmosSouthLongSlit(_, adv)) =>
      ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.fixedExposure
        )
        .getOption(adv)
        .map(r => ItcChartExposureTime(OverridenExposureTime.FromItc, r.time, r.count))
        .orElse(
          exposure.map(r => ItcChartExposureTime(OverridenExposureTime.Overriden, r.time, r.count))
        )

    case _ =>
      exposure
}

object ItcGraphPanel {
  private type Props = ItcGraphPanel

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(Pot.pending[Map[ItcTarget, ItcChartResult]])
      // loading
      .useState(PlotLoading.Done)
      // Request ITC graph data
      .useEffectWithDepsBy((props, _, _) =>
        (props.wavelength,
         props.scienceData.map(_.constraints),
         props.scienceData.flatMap(_.itcTargets.toNel),
         props.instrumentRow,
         props.chartExposureTime
        )
      ) {
        (props, charts, loading) =>
          (wavelength, constraints, itcTargets, instrumentRow, exposureTime) =>
            import props.given

            val action: Option[IO[Unit]] =
              for
                w           <- wavelength
                ex          <- exposureTime
                exposures   <- refineV[Positive](ex.count.value).toOption
                constraints <- constraints
                t           <- itcTargets
                mode        <- instrumentRow
              yield loading.setState(PlotLoading.Loading).to[IO] *>
                ItcClient[IO]
                  .request(
                    ItcMessage.GraphQuery(w, ex.time, exposures, constraints, t, mode)
                  )
                  .use(
                    _.evalMap(m =>
                      charts.modStateAsync {
                        case Pot.Ready(r) => Pot.Ready(r + (m.target -> m))
                        case u            => Pot.Ready(Map(m.target -> m))
                      } *> loading.setState(PlotLoading.Done).to[IO]
                    ).compile.drain
                  )
            action.getOrElse(
              (charts
                .setState(Pot.error(new RuntimeException("Not enough information to call ITC"))) *>
                loading
                  .setState(PlotLoading.Done))
                .to[IO]
            )
      }
      // Default selected chart
      .useStateView(ItcChartType.S2NChart)
      // show description
      .useStateView(PlotDetails.Shown)
      // selected target
      .useStateView(none[ItcTarget])
      .useEffectWithDepsBy((props, _, _, _, _, _) =>
        for
          w <- props.wavelength
          s <- props.scienceData
          t  = s.itcTargets
          b <- t.brightestAt(w)
        yield b
      )((_, _, _, _, _, selected) => t => selected.set(t))
      .render { (props, results, loading, chartType, details, selectedTarget) =>
        val error: Option[String] = results.value.fold(none, _.getMessage.some, _ => none)

        val selectedResult: Option[ItcChartResult] =
          selectedTarget.get.flatMap(t => results.value.toOption.flatMap(_.get(t)))

        <.div(
          ExploreStyles.ItcPlotSection,
          ExploreStyles.ItcPlotDetailsHidden.unless(details.when(_.value)),
          ItcSpectroscopyPlotDescription(selectedTarget,
                                         props.scienceData.foldMap(_.itcTargets),
                                         props.chartExposureTime,
                                         selectedResult.map(_.ccds)
          ),
          ItcSpectroscopyPlot(loading.value,
                              selectedResult.map(_.charts),
                              error,
                              chartType.get,
                              details.get
          ),
          ItcPlotControl(chartType, details)
        )
      }
}
