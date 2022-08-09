// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

// import cats.data._
// import cats.effect._
// import cats.effect.std.UUIDGen
import cats.syntax.all.*
// import coulomb.Quantity
// import coulomb.policy.spire.standard.given
// import crystal.react.View
// import crystal.react.hooks._
// import crystal.react.implicits._
// import crystal.react.reuse._
// import eu.timepit.refined.auto._
// import eu.timepit.refined.numeric.NonNegative
// import eu.timepit.refined.types.numeric.NonNegInt
// import eu.timepit.refined.types.numeric.PosBigDecimal
// import eu.timepit.refined.types.string.NonEmptyString
// import explore.Icons
import explore.common.ObsQueries._
// import explore.components.HelpIcon
// import explore.components.ui.ExploreStyles
// import explore.events.ItcQuery
// import explore.events._
// import explore.events.picklers._
import explore.implicits._
// import explore.itc._
// import explore.model.Progress
import explore.model.ScienceMode
// import explore.model.ScienceModeAdvanced
// import explore.model.ScienceModeBasic
// import explore.model.boopickle.Boopickle._
// import explore.model.boopickle._
// import explore.model.itc._
import explore.model.reusability._
// import explore.modes._
// import explore.syntax.ui.*
// import explore.syntax.ui.given
// import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
// import lucuma.core.enums.FocalPlane
// import lucuma.core.enums._
// import lucuma.core.math.Wavelength
// import lucuma.core.math.units.Micrometer
// import lucuma.core.model.ConstraintSet
// import lucuma.core.model.SiderealTracking
// import lucuma.core.util.Display
// import lucuma.refined.*
// import lucuma.ui.reusability._
// import lucuma.ui.syntax.all.*
// import lucuma.ui.syntax.all.given
// import react.CircularProgressbar.CircularProgressbar
// import react.common.Css
import react.common.ReactFnProps
// import explore.events.ItcGraphQuery
import cats.effect.std.UUIDGen
import cats.effect.IO
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.ScienceMode
import lucuma.core.math.Wavelength
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ConstraintSet
import queries.schemas.itc.implicits._
import explore.modes.InstrumentRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.GmosNorthSpectroscopyRow
import cats.data.OptionT
import explore.events.ItcGraphQuery
import cats.data.NonEmptyList
// import react.semanticui._
// import react.semanticui.collections.table._
// import react.semanticui.elements.button.Button
// import react.semanticui.elements.label.Label
// import react.semanticui.modules.popup.Popup
// import react.virtuoso._
// import react.virtuoso.raw.ListRange
// import reactST.reactTable._
// import reactST.reactTable.mod.DefaultSortTypes
// import reactST.reactTable.mod.SortByFn
// import reactST.reactTable.mod.UseTableRowProps
// import spire.math.Bounded
// import spire.math.Interval
//
// import java.text.DecimalFormat
// import java.util.UUID
//
// import scalajs.js.|
//
opaque type SignalToNoise = PosBigDecimal

final case class ItcGraphBody(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData]
)(implicit val ctx:         AppContextIO)
    extends ReactFnProps[ItcGraphBody](ItcGraphBody.component) {
  def wavelength: Option[Wavelength] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(_, adv: ScienceModeAdvanced.GmosNorthLongSlit)) =>
      adv.overrideWavelength.orElse(spectroscopyRequirements.flatMap(_.wavelength))
    case Some(ScienceMode.GmosSouthLongSlit(_, adv: ScienceModeAdvanced.GmosSouthLongSlit)) =>
      adv.overrideWavelength.orElse(spectroscopyRequirements.flatMap(_.wavelength))
    case _                                                                                  => none

  def signalToNoise: Option[PosBigDecimal] = scienceMode match
    case Some(ScienceMode.GmosNorthLongSlit(_, adv: ScienceModeAdvanced.GmosNorthLongSlit)) =>
      ScienceModeAdvanced.GmosNorthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.signalToNoiseValue
        )
        .getOption(adv)
        .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))
    case Some(ScienceMode.GmosSouthLongSlit(_, adv: ScienceModeAdvanced.GmosSouthLongSlit)) =>
      ScienceModeAdvanced.GmosSouthLongSlit.overrideExposureTimeMode.some
        .andThen(
          ExposureTimeMode.signalToNoiseValue
        )
        .getOption(adv)
        .orElse(spectroscopyRequirements.flatMap(_.signalToNoise))
    case _                                                                                  =>
      spectroscopyRequirements.flatMap(_.signalToNoise)

  def instrumentRow: Option[InstrumentRow] = scienceMode match
    case Some(
          ScienceMode.GmosNorthLongSlit(basic: ScienceModeBasic.GmosNorthLongSlit,
                                        adv: ScienceModeAdvanced.GmosNorthLongSlit
          )
        ) =>
      val grating = adv.overrideGrating.getOrElse(basic.grating)
      val filter  = adv.overrideFilter.orElse(basic.filter)
      val fpu     = adv.overrideFpu.getOrElse(basic.fpu)
      GmosNorthSpectroscopyRow(grating, fpu, filter).some
    case Some(
          ScienceMode.GmosSouthLongSlit(basic: ScienceModeBasic.GmosSouthLongSlit,
                                        adv: ScienceModeAdvanced.GmosSouthLongSlit
          )
        ) =>
      val grating = adv.overrideGrating.getOrElse(basic.grating)
      val filter  = adv.overrideFilter.orElse(basic.filter)
      val fpu     = adv.overrideFpu.getOrElse(basic.fpu)
      GmosSouthSpectroscopyRow(grating, fpu, filter).some
    case _ =>
      none
}

// sealed trait ItcPanel[T <: ScienceModeAdvanced, S <: ScienceModeBasic] {
//   val scienceModeAdvanced: T
//   val scienceModeBasic: S
//   val spectroscopyRequirements: SpectroscopyRequirementsData
//
//   implicit val ctx: AppContextIO
// }
//
// sealed abstract class ItcPanelBuilder[
//   T <: ScienceModeAdvanced,
//   S <: ScienceModeBasic: Reusability
// ]
//
// object ItcPanel {
//   sealed abstract class GmosAdvancedConfigurationPanel[
//     T <: ScienceModeAdvanced,
//     S <: ScienceModeBasic: Reusability
//   ] extends ItcPanelBuilder[T, S]
// }
//
// final case class GmosNorthLongSlit(
//   scienceModeAdvanced:      GmosNorthLongSlitAdvancedConfigInput
//   scienceModeBasic:         ScienceModeBasic.GmosNorthLongSlit,
//   spectroscopyRequirements: SpectroscopyRequirementsData,
//   potITC:                   View[Pot[Option[ITCSuccess]]],
//   editState:                View[ConfigEditState],
//   confMatrix:               SpectroscopyModesMatrix
// )(implicit val ctx:         AppContextIO)
//     extends ReactFnProps[AdvancedConfigurationPanel.GmosNorthLongSlit](
//       AdvancedConfigurationPanel.GmosNorthLongSlit.component
//     )
//     with AdvancedConfigurationPanel[
//       ScienceModeAdvanced.GmosNorthLongSlit,
//       ScienceModeBasic.GmosNorthLongSlit,
//       GmosNorthLongSlitAdvancedConfigInput,
//     ]
//
object ItcGraphBody {
  type Props = ItcGraphBody

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(true)
      .useEffectOnMount(Callback.log("alert!"))
      // Listen on web worker for messages with itc results candidates
      // .useStreamResourceBy((props, _, _, _, uuid) => (props.targets, uuid))(
      //   (props, _, itcCache, itcProgress, _) =>
      //     (_, uuid) =>
      //       props.ctx.worker.streamResource.map(
      //         _.map(decodeFromTransferable[WorkerMessage])
      //           .collect {
      //             case Some(ItcQueryResult(id, m)) if uuid.value.exists(_ === id) =>
      //               // Increment progress
      //               itcProgress
      //                 .modState {
      //                   // Remove progress when one left to complete
      //                   case Some(p) if p.nextToComplete => none
      //                   case Some(p)                     => Some(p.increment())
      //                   case _                           => none
      //                 }
      //                 .to[IO] *>
      //                 // Update the cache
      //                 itcCache.modState(_.update(m)).to[IO]
      //           }
      //           .evalMap(identity)
      //       )
      // )
      // Request ITC graph data
      .useEffectWithDepsBy((props, _) => props.scienceMode) { (props, _) => _ =>
        given AppContextIO = props.ctx

        (for
          uuid        <- OptionT.liftF(UUIDGen.fromSync[IO].randomUUID.map(_.some))
          w           <- OptionT.fromOption(props.wavelength)
          sn          <- OptionT.fromOption(props.signalToNoise)
          constraints <- OptionT.fromOption(props.scienceData.map(_.constraints))
          t           <-
            OptionT.fromOption(props.scienceData.flatMap(r => NonEmptyList.fromList(r.itcTargets)))
          mode        <- OptionT.fromOption(props.instrumentRow)
        yield {
          println(mode)
          println(uuid)
          uuid
            .map(u =>
              IO.println("CALL") *> props.ctx.worker
                .postWorkerMessage(ItcGraphQuery(u, w, sn, constraints, t, mode))
            )
            .orEmpty
        }).getOrElse(IO.unit).flatten
      }
      //   println("scienceMode")
      //   Callback.log("Run")
      //   // val sortedRows = ti.value.preSortedRows.map(_.original).toList
      //   //
      //   // (wavelength, signalToNoise, targets.flatMap(NonEmptyList.fromList)).mapN { (w, sn, t) =>
      //   //   // Discard modes already in the cache
      //   //   val modes =
      //   //     (range.value.foldMap(visibleRows(_, sortedRows)) ++ sortedRows).distinct.filterNot {
      //   //       row =>
      //   //         val cache = itcResults.value.cache
      //   //         row.instrument match
      //   //           case m: GmosNorthSpectroscopyRow =>
      //   //             cache.contains(
      //   //               ItcRequestParams(w, sn, constraints, t, m)
      //   //             )
      //   //           case m: GmosSouthSpectroscopyRow =>
      //   //             cache.contains(
      //   //               ItcRequestParams(w, sn, constraints, t, m)
      //   //             )
      //   //           case _                           => true
      //   //     }
      //   //
      //   //   val queryable =
      //   //     modes.length === 1 || (modes.length > 1 && range.value.exists(_.isDefined))
      //   //
      //   //   val progress = Progress.initial(NonNegInt.unsafeFrom(modes.length)).some
      //   //   // new request ID, we are inside the effect
      //   //   (for
      //   //     uuid <- UUIDGen.fromSync[IO].randomUUID
      //   //     _    <- itcProgress.setState(progress).to[IO]
      //   //     _    <- queryId.setState(uuid.some).to[IO]
      //   //     _    <-
      //   //       props.ctx.worker.postWorkerMessage(ItcQuery(uuid, w, sn, constraints, t, modes))
      //   //   yield ()).whenA(queryable)
      //   // }.orEmpty
      // }
      .render { (props, _) =>
        <.div(s"itc ${props.scienceMode}")
      }
}
