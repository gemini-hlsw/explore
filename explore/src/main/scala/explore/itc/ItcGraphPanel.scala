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
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.UnderConstruction
import explore.common.ObsQueries._
import explore.components.WIP
import explore.events._
import explore.implicits._
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle._
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.ItcChart
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
  scienceData:              Option[ScienceData]
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
}

object ItcGraphPanel {
  type Props = ItcGraphPanel

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(Pot.pending[List[ItcChart]])
      // Request ITC graph data
      .useEffectWithDepsBy((props, _) =>
        (props.wavelength,
         props.signalToNoise,
         props.scienceData.map(_.constraints),
         props.scienceData.flatMap(_.itcTargets.toNel),
         props.instrumentRow
        )
      ) { (props, charts) => (wavelength, signalToNoise, constraints, itcTargets, instrumentRow) =>
        given AppContextIO = props.ctx

        (for
          w           <- OptionT.fromOption[IO](wavelength)
          sn          <- OptionT.fromOption[IO](signalToNoise)
          constraints <- OptionT.fromOption[IO](constraints)
          t           <- OptionT.fromOption[IO](itcTargets)
          mode        <- OptionT.fromOption[IO](instrumentRow)
        yield ItcClient[IO]
          .requestSingle(ItcMessage.GraphQuery(w, sn, constraints, t, mode))
          .flatMap(_.map(m => charts.setStateAsync(Pot.Ready(m))).orEmpty))
          .getOrElse(IO.unit)
          .flatten
      }
      .render { (_, charts) =>
        ItcSpectroscopyPlot(charts.value)
      }
}
