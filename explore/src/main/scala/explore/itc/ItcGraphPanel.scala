// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

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
import explore.components.ui.ExploreStyles
import explore.events.ItcGraphQuery
import explore.events._
import explore.events.picklers._
import explore.implicits._
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.boopickle.Boopickle._
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
    case _                                           => none

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
    case _                                           =>
      spectroscopyRequirements.flatMap(_.signalToNoise)

  def instrumentRow: Option[InstrumentRow] = scienceMode match
    case Some(
          ScienceMode.GmosNorthLongSlit(basic, adv)
        ) =>
      val grating = adv.overrideGrating.getOrElse(basic.grating)
      val filter  = adv.overrideFilter.orElse(basic.filter)
      val fpu     = adv.overrideFpu.getOrElse(basic.fpu)
      GmosNorthSpectroscopyRow(grating, fpu, filter).some
    case Some(
          ScienceMode.GmosSouthLongSlit(basic, adv)
        ) =>
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
      // itc requests id
      .useState(none[UUID])
      .useState(Pot.pending[List[ItcChart]])
      // Listen on web worker for messages with itc results candidates
      .useStreamResourceBy((_, uuid, _) => uuid)((props, _, charts) =>
        uuid =>
          props.ctx.worker.streamResource.map(
            _.map(decodeFromTransferable[WorkerMessage])
              .collect {
                case Some(ItcGraphResult(id, m)) if uuid.value.exists(_ === id) =>
                  charts.setState(Pot.Ready(m)).to[IO]
              }
              .evalMap(identity)
          )
      )
      // Request ITC graph data
      .useEffectWithDepsBy((props, _, _, _) =>
        (props.wavelength,
         props.signalToNoise,
         props.scienceData.map(_.constraints),
         props.scienceData.flatMap(_.itcTargets.toNel),
         props.instrumentRow
        )
      ) {
        (props, queryId, _, _) =>
          (wavelength, signalToNoise, constraints, itcTargets, instrumentRow) =>
            given AppContextIO = props.ctx

            (for
              uuid        <- OptionT.liftF(UUIDGen.fromSync[IO].randomUUID.map(_.some))
              w           <- OptionT.fromOption(wavelength)
              sn          <- OptionT.fromOption(signalToNoise)
              constraints <- OptionT.fromOption(constraints)
              t           <- OptionT.fromOption(itcTargets)
              mode        <- OptionT.fromOption(instrumentRow)
            yield uuid
              .map(u =>
                queryId.setState(uuid).to[IO] *> props.ctx.worker
                  .postWorkerMessage(ItcGraphQuery(u, w, sn, constraints, t, mode))
              )
              .orEmpty).getOrElse(IO.unit).flatten
      }
      .render { (_, _, charts, _) =>
        ItcSpectroscopyPlot(charts.value)
      }
}
