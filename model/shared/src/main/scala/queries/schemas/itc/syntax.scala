// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.itc

import cats.Hash
import cats.syntax.all.*
import explore.model.AsterismIds
import explore.model.TargetList
import explore.model.itc.ItcTarget
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import explore.optics.all.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.math.RadialVelocity
import lucuma.core.model.*
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.binning.*
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.InstrumentMode

trait syntax:

  extension (row: InstrumentRow)
    def toItcClientMode(p: SourceProfile, iq: ImageQuality): Option[InstrumentMode] = row match {
      case g: GmosNorthSpectroscopyRow =>
        val isIFU        =
          g.fpu === GmosNorthFpu.Ifu2Slits || g.fpu === GmosNorthFpu.IfuBlue || g.fpu === GmosNorthFpu.IfuRed
        val (xbin, ybin) =
          if (isIFU) (GmosXBinning.One, GmosYBinning.One)
          else northBinning(g.fpu, p, iq, g.grating)
        val roi          = g.modeOverrides.flatMap(_.roi).orElse(DefaultRoi.some)
        val ccd          = g.modeOverrides
          .flatMap(_.ccdMode)
          .orElse(
            GmosCcdMode(
              xbin,
              ybin,
              DefaultAmpCount,
              DefaultAmpGain,
              DefaultAmpReadMode
            ).some
          )
        InstrumentMode
          .GmosNorthSpectroscopy(g.grating, g.filter, GmosFpu.North(g.fpu.asRight), ccd, roi)
          .some
      case g: GmosSouthSpectroscopyRow =>
        val isIFU        =
          g.fpu === GmosSouthFpu.Ifu2Slits || g.fpu === GmosSouthFpu.IfuBlue || g.fpu === GmosSouthFpu.IfuRed
        val (xbin, ybin) =
          if (isIFU) (GmosXBinning.One, GmosYBinning.One)
          else southBinning(g.fpu, p, iq, g.grating)
        val roi          = g.modeOverrides.flatMap(_.roi).orElse(DefaultRoi.some)
        val ccd          = g.modeOverrides
          .flatMap(_.ccdMode)
          .orElse(
            GmosCcdMode(
              xbin,
              ybin,
              DefaultAmpCount,
              DefaultAmpGain,
              DefaultAmpReadMode
            ).some
          )
        InstrumentMode
          .GmosSouthSpectroscopy(g.grating, g.filter, GmosFpu.South(g.fpu.asRight), ccd, roi)
          .some
      case _                           => None
    }

  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  private given Hash[RadialVelocity] = Hash.by(_.rv.value)
  private given Hash[SourceProfile]  = Hash.fromUniversalHashCode
  private given Hash[ItcTarget]      = Hash.by(x => (x.name.value, x.rv, x.profile))

  extension (targetIds: AsterismIds)
    // From the list of targets selects the ones relevant for ITC
    def itcTargets(allTargets: TargetList): List[ItcTarget] =
      targetIds.toList
        .map(targetId =>
          allTargets
            .get(targetId)
            .flatMap(target =>
              targetRV
                .getOption(target)
                .map(r => ItcTarget(target.name, r, Target.sourceProfile.get(target)))
            )
        )
        .flatten
        .hashDistinct

object syntax extends syntax
