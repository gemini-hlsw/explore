// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import lucuma.core.model.sequence.gmos.GmosCcdMode
import cats.data.NonEmptyList
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import cats.syntax.all.*
import lucuma.core.model.SourceProfile
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating

object syntax:
  // TODO Actually move to lucuma-core, to GmosCcdMode
  extension (self: GmosCcdMode.type)
    // For multiple targets, we take the smallest binning for each axis.
    // https://docs.google.com/document/d/1P8_pXLRVomUSvofyVkAniOyGROcAtiJ7EMYt9wWXB0o/edit?disco=AAAA32SmtD4
    private def asterismBinning(
      bs: NonEmptyList[(GmosXBinning, GmosYBinning)]
    ): (GmosXBinning, GmosYBinning) =
      (bs.map(_._1).minimumBy(_.count), bs.map(_._2).minimumBy(_.count))

    def defaultGmosNorth(
      profiles:     NonEmptyList[SourceProfile],
      fpu:          GmosNorthFpu,
      grating:      GmosNorthGrating,
      imageQuality: ImageQuality
    ): GmosCcdMode =
      val (defaultXBinning, defaultYBinning) =
        if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
        else asterismBinning(profiles.map(northBinning(fpu, _, imageQuality, grating)))

      GmosCcdMode(
        defaultXBinning,
        defaultYBinning,
        DefaultAmpCount,
        DefaultAmpGain,
        DefaultAmpReadMode
      )

    def defaultGmosSouth(
      profiles:     NonEmptyList[SourceProfile],
      fpu:          GmosSouthFpu,
      grating:      GmosSouthGrating,
      imageQuality: ImageQuality
    ): GmosCcdMode =
      val (defaultXBinning, defaultYBinning) =
        if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
        else asterismBinning(profiles.map(southBinning(fpu, _, imageQuality, grating)))

      GmosCcdMode(
        defaultXBinning,
        defaultYBinning,
        DefaultAmpCount,
        DefaultAmpGain,
        DefaultAmpReadMode
      )
