// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import explore.model.itc.CoverageCenterWavelength
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.schemas.decoders.*
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

// The information required to send to the API to create a configuration.
sealed abstract class ScienceModeInitial(val instrument: Instrument)
    extends Product
    with Serializable derives Eq

object ScienceModeInitial:
  case class GmosNorthLongSlit(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: CoverageCenterWavelength
  ) extends ScienceModeInitial(Instrument.GmosNorth)
      derives Eq

  case class GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CoverageCenterWavelength
  ) extends ScienceModeInitial(Instrument.GmosSouth)
      derives Eq
