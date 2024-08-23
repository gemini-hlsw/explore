// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas.itc

import cats.Hash
import cats.data.NonEmptyList
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
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.TargetInput

trait syntax:

  // For multiple targets, we take the smallest binning for each axis.
  // https://docs.google.com/document/d/1P8_pXLRVomUSvofyVkAniOyGROcAtiJ7EMYt9wWXB0o/edit?disco=AAAA32SmtD4
  private def asterismBinning(
    bs: NonEmptyList[(GmosXBinning, GmosYBinning)]
  ): (GmosXBinning, GmosYBinning) =
    (bs.map(_._1).minimumBy(_.count), bs.map(_._2).minimumBy(_.count))

  extension (row: InstrumentRow)
    def toItcClientMode(ps: NonEmptyList[SourceProfile], iq: ImageQuality): Option[InstrumentMode] =
      row match {
        case GmosNorthSpectroscopyRow(grating, fpu, filter, modeOverrides) =>
          val (xbin, ybin) =
            if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
            else asterismBinning(ps.map(northBinning(fpu, _, iq, grating)))
          val roi          = modeOverrides.flatMap(_.roi).orElse(DefaultRoi.some)
          val ccd          = modeOverrides
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
            .GmosNorthSpectroscopy(grating, filter, GmosFpu.North(fpu.asRight), ccd, roi)
            .some
        case GmosSouthSpectroscopyRow(grating, fpu, filter, modeOverrides) =>
          val (xbin, ybin) =
            if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
            else asterismBinning(ps.map(southBinning(fpu, _, iq, grating)))
          val roi          = modeOverrides.flatMap(_.roi).orElse(DefaultRoi.some)
          val ccd          = modeOverrides
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
            .GmosSouthSpectroscopy(grating, filter, GmosFpu.South(fpu.asRight), ccd, roi)
            .some
        case _                                                             => None
      }

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

object syntax extends syntax
