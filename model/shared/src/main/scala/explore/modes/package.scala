// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Order
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.optics.Wedge
import lucuma.core.util.NewBoolean
import lucuma.core.util.NewType

object ModeWavelength extends NewType[Wavelength]
type ModeWavelength = ModeWavelength.Type

object ModeSlitSize extends NewType[Angle]:
  val milliarcseconds: Wedge[Angle, BigDecimal] =
    Angle.milliarcseconds
      .imapB(_.underlying.movePointRight(3).intValue,
             n => new java.math.BigDecimal(n).movePointLeft(3)
      )

  given Order[ModeSlitSize] = Order.by(_.value.toMicroarcseconds)

type ModeSlitSize = ModeSlitSize.Type

object ModeAO extends NewBoolean { inline def AO = True; inline def NoAO = False }
type ModeAO = ModeAO.Type
