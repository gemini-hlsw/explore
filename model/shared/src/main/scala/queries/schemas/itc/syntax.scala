// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.itc

import cats.Hash
import cats.syntax.all.*
import explore.model.AsterismIds
import explore.model.TargetList
import explore.model.itc.ItcTarget
import explore.modes.ItcInstrumentConfig
import explore.optics.all.*
import lucuma.core.enums.GmosRoi
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.model.*
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.TargetInput

trait syntax:

  extension (row: ItcInstrumentConfig)
    def toItcClientMode: Option[InstrumentMode] =
      row match
        case ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, filter, modeOverrides) =>
          val roi: Option[GmosRoi]     = modeOverrides.map(_.roi)
          val ccd: Option[GmosCcdMode] = modeOverrides.map(_.ccdMode)
          modeOverrides
            .map(_.centralWavelength.value)
            .flatMap: (cw: Wavelength) =>
              InstrumentMode
                .GmosNorthSpectroscopy(cw, grating, filter, GmosFpu.North(fpu.asRight), ccd, roi)
                .some
        case ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, filter, modeOverrides) =>
          val roi: Option[GmosRoi]     = modeOverrides.map(_.roi)
          val ccd: Option[GmosCcdMode] = modeOverrides.map(_.ccdMode)
          modeOverrides
            .map(_.centralWavelength.value)
            .flatMap: (cw: Wavelength) =>
              InstrumentMode
                .GmosSouthSpectroscopy(cw, grating, filter, GmosFpu.South(fpu.asRight), ccd, roi)
                .some
        case ItcInstrumentConfig.Flamingos2Spectroscopy(disperser, filter, fpu)             =>
          InstrumentMode
            .Flamingos2Spectroscopy(disperser, filter, fpu)
            .some
        case _                                                                              => None

  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  private given Hash[RadialVelocity] = Hash.by(_.rv.value)
  private given Hash[SourceProfile]  = Hash.fromUniversalHashCode
  private given Hash[TargetInput]    = Hash.by(x => (x.sourceProfile, x.radialVelocity))
  private given Hash[ItcTarget]      = Hash.by(x => (x.name.value, x.input))

  extension (targetIds: AsterismIds)
    // From the list of targets selects the ones relevant for ITC
    def itcTargets(allTargets: TargetList): List[ItcTarget] =
      targetIds.toList
        .map(targetId =>
          allTargets
            .get(targetId)
            .flatMap(target =>
              TargetRV
                .getOption(target)
                .map(r => ItcTarget(target.name, TargetInput(Target.sourceProfile.get(target), r)))
            )
        )
        .flatten
        .hashDistinct

  extension (allTargets: TargetList)
    // From the list of targets selects the ones relevant for ITC
    def itcTargets: List[ItcTarget] =
      allTargets.values
        .map(target =>
          TargetRV
            .getOption(target)
            .map(r => ItcTarget(target.name, TargetInput(Target.sourceProfile.get(target), r)))
        )
        .toList
        .flattenOption
        .hashDistinct

object syntax extends syntax
