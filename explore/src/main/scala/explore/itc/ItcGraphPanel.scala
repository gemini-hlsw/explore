// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic._
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.IO
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import crystal.Pot
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.UnderConstruction
import explore.common.ObsQueries._
import explore.components.WIP
import explore.components.ui.ExploreStyles
import explore.config.ExposureTimeModeType.FixedExposure
import explore.events._
import explore.implicits._
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle._
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.ItcSeries
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.OverridenExposureTime
import explore.model.reusability._
import explore.model.reusability.given
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.given
import queries.schemas.itc.implicits._
import react.common.ReactFnProps

import java.util.UUID

final case class ItcGraphPanel(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime]
)(using val ctx:            AppContextIO)
    extends ReactFnProps[ItcGraphPanel](ItcGraphPanel.component) {
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
  type Props = ItcGraphPanel

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(Pot.pending[ItcChartResult])
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
                  .requestSingle(
                    ItcMessage.GraphQuery(w, ex.time, exposures, constraints, t, mode)
                  )
                  .flatMap(
                    _.map(m =>
                      charts.setStateAsync(Pot.Ready(m)) *>
                        loading.setState(PlotLoading.Done).to[IO]
                    ).orEmpty
                  )
            action.getOrElse(
              (charts
                .setState(Pot.error(new RuntimeException("Not enough information to call ITC"))) *>
                loading
                  .setState(PlotLoading.Done))
                .to[IO]
            )
      }
      .render { (props, results, loading) =>
        val error: Option[String] = results.value.fold(none, _.getMessage.some, _ => none)
        <.div(
          ExploreStyles.ItcPlotSection,
          ItcSpectroscopyPlotDescription(props.chartExposureTime, results.value.map(_.ccds)),
          ItcSpectroscopyPlot(loading.value, results.value.map(_.charts), error)
        )
      }
}
