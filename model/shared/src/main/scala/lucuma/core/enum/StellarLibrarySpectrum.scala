// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Enumerated

sealed abstract class StellarLibrarySpectrum(
  val tag:         String,
  val sedSpectrum: String,
  val ocs2Tag:     String
) extends Product
    with Serializable

object StellarLibrarySpectrum {

  case object O5V     extends StellarLibrarySpectrum("O5V", "O5V", "O5V")
  case object O8III   extends StellarLibrarySpectrum("O8III", "O8III", "O8III")
  case object B0V     extends StellarLibrarySpectrum("B0V", "B0V", "B0V")
  case object B5_7V   extends StellarLibrarySpectrum("B5_7V", "B5-7V", "B5_7V")
  case object B5III   extends StellarLibrarySpectrum("B5III", "B5III", "B5III")
  case object B5I     extends StellarLibrarySpectrum("B5I", "B5I", "B5I")
  case object A0V     extends StellarLibrarySpectrum("A0V", "A0V", "A0V")
  case object A0III   extends StellarLibrarySpectrum("A0III", "A0III", "A0III")
  case object A0I     extends StellarLibrarySpectrum("A0I", "A0I", "A0I")
  case object A5V     extends StellarLibrarySpectrum("A5V", "A5V", "A5V")
  case object A5III   extends StellarLibrarySpectrum("A5III", "A5III", "A5III")
  case object F0V     extends StellarLibrarySpectrum("F0V", "F0V", "F0V")
  case object F0III   extends StellarLibrarySpectrum("F0III", "F0III", "F0III")
  case object F0I     extends StellarLibrarySpectrum("F0I", "F0I", "F0I")
  case object F5V     extends StellarLibrarySpectrum("F5V", "F5V", "F5V")
  case object F5V_w   extends StellarLibrarySpectrum("F5V_w", "F5V-w", "F5V_w")
  case object F6V_r   extends StellarLibrarySpectrum("F6V_r", "F6V-r", "F6V_r")
  case object F5III   extends StellarLibrarySpectrum("F5III", "F5III", "F5III")
  case object F5I     extends StellarLibrarySpectrum("F5I", "F5I", "F5I")
  case object G0V     extends StellarLibrarySpectrum("G0V", "G0V", "G0V")
  case object G0V_w   extends StellarLibrarySpectrum("G0V_w", "G0V-w", "G0V_w")
  case object G0V_r   extends StellarLibrarySpectrum("G0V_r", "G0V-r", "G0V_r")
  case object G0III   extends StellarLibrarySpectrum("G0III", "G0III", "G0III")
  case object G0I     extends StellarLibrarySpectrum("G0I", "G0I", "G0I")
  case object G2V     extends StellarLibrarySpectrum("G2V", "G2V", "G2V")
  case object G5V     extends StellarLibrarySpectrum("G5V", "G5V", "G5V")
  case object G5V_w   extends StellarLibrarySpectrum("G5V_w", "G5V-w", "G5V_w")
  case object G5V_r   extends StellarLibrarySpectrum("G5V_r", "G5V-r", "G5V_r")
  case object G5III   extends StellarLibrarySpectrum("G5III", "G5III", "G5III")
  case object G5III_w extends StellarLibrarySpectrum("G5III_w", "G5III-w", "G5III_w")
  case object G5III_r extends StellarLibrarySpectrum("G5III_r", "G5III-r", "G5III_r")
  case object G5I     extends StellarLibrarySpectrum("G5I", "G5I", "G5I")
  case object K0V     extends StellarLibrarySpectrum("K0V", "K0V", "K0V")
  case object K0V_r   extends StellarLibrarySpectrum("K0V_r", "K0V-r", "K0V_r")
  case object K0III   extends StellarLibrarySpectrum("K0III", "K0III", "K0III")
  case object K0III_w extends StellarLibrarySpectrum("K0III_w", "K0III-w", "K0III_w")
  case object K0III_r extends StellarLibrarySpectrum("K0III_r", "K0III-r", "K0III_r")
  case object K0_1II  extends StellarLibrarySpectrum("K0_1II", "K0-1II", "K0_1II")
  case object K4V     extends StellarLibrarySpectrum("K4V", "K4V", "K4V")
  case object K4III   extends StellarLibrarySpectrum("K4III", "K4III", "K4III")
  case object K4III_w extends StellarLibrarySpectrum("K4III_w", "K4III-w", "K4III_w")
  case object K4III_r extends StellarLibrarySpectrum("K4III_r", "K4III-r", "K4III_r")
  case object K4I     extends StellarLibrarySpectrum("K4I", "K4I", "K4I")
  case object M0V     extends StellarLibrarySpectrum("M0V", "M0V", "M0V")
  case object M0III   extends StellarLibrarySpectrum("M0III", "M0III", "M0III")
  case object M3V     extends StellarLibrarySpectrum("M3V", "M3V", "M3V")
  case object M3III   extends StellarLibrarySpectrum("M3III", "M3III", "M3III")
  case object M6V     extends StellarLibrarySpectrum("M6V", "M6V", "M6V")
  case object M6III   extends StellarLibrarySpectrum("M6III", "M6III", "M6III")
  case object M9III   extends StellarLibrarySpectrum("M9III", "M9III", "M9III")

  /** @group Typeclass Instances */
  implicit val StellarLibrarySpectrumEnumerated: Enumerated[StellarLibrarySpectrum] =
    Enumerated.of(
      O5V,
      O8III,
      B0V,
      B5_7V,
      B5III,
      B5I,
      A0V,
      A0III,
      A0I,
      A5V,
      A5III,
      F0V,
      F0III,
      F0I,
      F5V,
      F5V_w,
      F6V_r,
      F5III,
      F5I,
      G0V,
      G0V_w,
      G0V_r,
      G0III,
      G0I,
      G2V,
      G5V,
      G5V_w,
      G5V_r,
      G5III,
      G5III_w,
      G5III_r,
      G5I,
      K0V,
      K0V_r,
      K0III,
      K0III_w,
      K0III_r,
      K0_1II,
      K4V,
      K4III,
      K4III_w,
      K4III_r,
      K4I,
      M0V,
      M0III,
      M3V,
      M3III,
      M6V,
      M6III,
      M9III
    )

}
