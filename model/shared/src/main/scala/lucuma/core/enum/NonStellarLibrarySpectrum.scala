// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Enumerated

sealed abstract class NonStellarLibrarySpectrum(
  val tag:         String,
  val shortName:   String,
  val sedSpectrum: String,
  val ocs2Tag:     String
) extends Product
    with Serializable

object NonStellarLibrarySpectrum {

  case object EllipticalGalaxy
      extends NonStellarLibrarySpectrum("EllipticalGalaxy",
                                        "Elliptical Galaxy",
                                        "elliptical-galaxy",
                                        "EllipticalGalaxy"
      )
  case object SpiralGalaxy
      extends NonStellarLibrarySpectrum("SpiralGalaxy",
                                        "Spiral Galaxy (Sc)",
                                        "spiral-galaxy",
                                        "SpiralGalaxy"
      )
  case object QS0     extends NonStellarLibrarySpectrum("QS0", "QSO (80-855nm)", "QSO", "QS0")
  case object QS02    extends NonStellarLibrarySpectrum("QS02", "QSO (276-3520nm)", "QSO2", "QS02")
  case object OrionNebula
      extends NonStellarLibrarySpectrum("OrionNebula",
                                        "HII region (Orion)",
                                        "Orion-nebula",
                                        "OrionNebula"
      )
  case object PlanetaryNebula
      extends NonStellarLibrarySpectrum("PlanetaryNebula",
                                        "Planetary nebula (NGC7009: 100-1100nm)",
                                        "Planetary-nebula",
                                        "PlanetaryNebula"
      )
  case object PlanetaryNebula2
      extends NonStellarLibrarySpectrum("PlanetaryNebula2",
                                        "Planetary nebula (IC5117: 480-2500nm)",
                                        "Planetary-nebula2",
                                        "PlanetaryNebula2"
      )
  case object PlanetaryNebula3
      extends NonStellarLibrarySpectrum("PlanetaryNebula3",
                                        "Planetary nebula (NGC7027)",
                                        "Planetary-nebula-NGC7027",
                                        "PlanetaryNebula3"
      )
  case object StarburstGalaxy
      extends NonStellarLibrarySpectrum("StarburstGalaxy",
                                        "Starburst galaxy (M82)",
                                        "Starburst-galaxy",
                                        "StarburstGalaxy"
      )
  case object PmsStar
      extends NonStellarLibrarySpectrum("PmsStar",
                                        "Pre-main sequence star (HD100546)",
                                        "PMS-star",
                                        "PmsStar"
      )
  case object GalacticCenter
      extends NonStellarLibrarySpectrum("GalacticCenter",
                                        "Galactic center",
                                        "Galactic-center",
                                        "GalacticCenter"
      )
  case object Afgl230
      extends NonStellarLibrarySpectrum("Afgl230",
                                        "AFGL230 (M10II star, silicate absorp.)",
                                        "afgl230",
                                        "Afgl230"
      )
  case object Afgl3068
      extends NonStellarLibrarySpectrum("Afgl3068",
                                        "AFGL3068 (Late N-type star)",
                                        "afgl3068",
                                        "Afgl3068"
      )
  case object AlphaBoo
      extends NonStellarLibrarySpectrum("AlphaBoo",
                                        "Alpha Boo (K1.5III star)",
                                        "alphaboo",
                                        "AlphaBoo"
      )
  case object AlphaCar
      extends NonStellarLibrarySpectrum("AlphaCar", "Alpha Car (F0II star)", "alphacar", "AlphaCar")
  case object BetaAnd
      extends NonStellarLibrarySpectrum("BetaAnd", "Beta And (M0IIIa star)", "betaand", "BetaAnd")
  case object BetaGru
      extends NonStellarLibrarySpectrum("BetaGru", "Beta Gru (M5III star)", "betagru", "BetaGru")
  case object GammaCas
      extends NonStellarLibrarySpectrum("GammaCas",
                                        "Gamma Cas (B0IVe star)",
                                        "gammacas",
                                        "GammaCas"
      )
  case object GammaDra
      extends NonStellarLibrarySpectrum("GammaDra",
                                        "Gamma Dra (K5III star)",
                                        "gammadra",
                                        "GammaDra"
      )
  case object L1511Irs
      extends NonStellarLibrarySpectrum("L1511Irs",
                                        "l1551irs (young stellar object)",
                                        "l1551irs",
                                        "L1511Irs"
      )
  case object NGC1068
      extends NonStellarLibrarySpectrum("NGC1068",
                                        "NGC 1068 (Dusty active galaxy)",
                                        "ngc1068",
                                        "NGC1068"
      )
  case object NGC2023
      extends NonStellarLibrarySpectrum("NGC2023",
                                        "NGC2023 (Reflection Nebula)",
                                        "ngc2023",
                                        "NGC2023"
      )
  case object NGC2440
      extends NonStellarLibrarySpectrum("NGC2440",
                                        "NGC2440 (line dominated PN)",
                                        "ngc2440",
                                        "NGC2440"
      )
  case object OCet
      extends NonStellarLibrarySpectrum("OCet",
                                        "O Cet (M7IIIa Star, silicate emission)",
                                        "ocet",
                                        "OCet"
      )
  case object OrionBar
      extends NonStellarLibrarySpectrum("OrionBar",
                                        "Orion Bar (Dusty HII region)",
                                        "orionbar",
                                        "OrionBar"
      )
  case object Rscl
      extends NonStellarLibrarySpectrum("Rscl",
                                        "rscl (N-type Dusty Carbon Star, SiC em.)",
                                        "rscl",
                                        "Rscl"
      )
  case object Txpsc
      extends NonStellarLibrarySpectrum("Txpsc",
                                        "txpsc (N-type Visible Carbon Star)",
                                        "txpsc",
                                        "Txpsc"
      )
  case object Wr104
      extends NonStellarLibrarySpectrum("Wr104",
                                        "WR 104 (Wolf-Rayet Star + dust)",
                                        "wr104",
                                        "Wr104"
      )
  case object Wr34
      extends NonStellarLibrarySpectrum("Wr34", "WR 34 (Wolf-Rayet Star)", "wr34", "Wr34")
  case object Mars    extends NonStellarLibrarySpectrum("Mars", "Mars", "Mars", "Mars")
  case object Jupiter extends NonStellarLibrarySpectrum("Jupiter", "Jupiter", "Jupiter", "Jupiter")
  case object Saturn  extends NonStellarLibrarySpectrum("Saturn", "Saturn", "Saturn", "Saturn")
  case object Uranus  extends NonStellarLibrarySpectrum("Uranus", "Uranus", "Uranus", "Uranus")
  case object Neptune extends NonStellarLibrarySpectrum("Neptune", "Neptune", "Neptune", "Neptune")

  /** @group Typeclass Instances */
  implicit val NonStellarLibrarySpectrumEnumerated: Enumerated[NonStellarLibrarySpectrum] =
    Enumerated.of(
      EllipticalGalaxy,
      SpiralGalaxy,
      QS0,
      QS02,
      OrionNebula,
      PlanetaryNebula,
      PlanetaryNebula2,
      PlanetaryNebula3,
      StarburstGalaxy,
      PmsStar,
      GalacticCenter,
      Afgl230,
      Afgl3068,
      AlphaBoo,
      AlphaCar,
      BetaAnd,
      BetaGru,
      GammaCas,
      GammaDra,
      L1511Irs,
      NGC1068,
      NGC2023,
      NGC2440,
      OCet,
      OrionBar,
      Rscl,
      Txpsc,
      Wr104,
      Wr34,
      Mars,
      Jupiter,
      Saturn,
      Uranus,
      Neptune
    )

}
