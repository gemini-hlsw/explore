// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class Keyword(val label: String) extends Product with Serializable

object Keyword {
  case object AbsorptionLines              extends Keyword("Absorption lines")
  case object Accretion                    extends Keyword("Accretion")
  case object ActiveGalaxies               extends Keyword("Active galaxies")
  case object AgbStars                     extends Keyword("AGB stars")
  case object Asteroids                    extends Keyword("Asteroids")
  case object Astrobiology                 extends Keyword("Astrobiology")
  case object Astrochemistry               extends Keyword("Astrochemistry")
  case object Astrometry                   extends Keyword("Astrometry")
  case object Atmospheres                  extends Keyword("Atmospheres")
  case object Binaries                     extends Keyword("Binaries")
  case object BlackHolePhysics             extends Keyword("Black hole physics")
  case object BlueStragglers               extends Keyword("Blue stragglers")
  case object BrownDwarfs                  extends Keyword("Brown dwarfs")
  case object Bulges                       extends Keyword("Bulges")
  case object CarbonStars                  extends Keyword("Carbon stars")
  case object CataclysmicVariables         extends Keyword("Cataclysmic variables")
  case object ChemicalAbundances           extends Keyword("Chemical abundances")
  case object CircumstellarMatter          extends Keyword("Circumstellar matter")
  case object Comets                       extends Keyword("Comets")
  case object CoolingFlows                 extends Keyword("Cooling flows")
  case object CosmologicalParameters       extends Keyword("Cosmological parameters")
  case object DarkMatter                   extends Keyword("Dark matter")
  case object DiffuseFadiation             extends Keyword("Diffuse radiation")
  case object Distances                    extends Keyword("Distances")
  case object Dust                         extends Keyword("Dust")
  case object DwarfGalaxies                extends Keyword("Dwarf galaxies")
  case object DwarfNovae                   extends Keyword("Dwarf novae")
  case object Dynamics                     extends Keyword("Dynamics")
  case object EarlyTypeStars               extends Keyword("Early-type stars")
  case object EarlyUniverse                extends Keyword("Early universe")
  case object Earth                        extends Keyword("Earth")
  case object EllipticalGalaxies           extends Keyword("Elliptical galaxies")
  case object EmissionLines                extends Keyword("Emission lines")
  case object Evolution                    extends Keyword("Evolution")
  case object Extinction                   extends Keyword("Extinction")
  case object Formation                    extends Keyword("Formation")
  case object FundamentalParameters        extends Keyword("Fundamental parameters")
  case object GalaxyClusters               extends Keyword("Galaxy clusters")
  case object GalacticStructure            extends Keyword("Galactic structure")
  case object GammaRayBursts               extends Keyword("Gamma ray bursts")
  case object GlobularClusters             extends Keyword("Globular clusters")
  case object GravitationalLensing         extends Keyword("Gravitational lensing")
  case object GroupsOfGalaxies             extends Keyword("Groups of galaxies")
  case object HIIRegions                   extends Keyword("H II regions")
  case object Halos                        extends Keyword("Halos")
  case object HerbigHaroObjects            extends Keyword("Herbig-Haro objects")
  case object HighRedshift                 extends Keyword("High-redshift")
  case object HorizontalBranchStars        extends Keyword("Horizontal-branch stars")
  case object InteractingGalaxies          extends Keyword("Interacting galaxies")
  case object IntergalacticMedium          extends Keyword("Intergalactic medium")
  case object InterplanetaryMedium         extends Keyword("Interplanetary medium")
  case object InterstellarMedium           extends Keyword("Interstellar medium")
  case object IrregularGalaxies            extends Keyword("Irregular galaxies")
  case object Jets                         extends Keyword("Jets")
  case object KuiperBelt                   extends Keyword("Kuiper Belt")
  case object LargeScaleStructure          extends Keyword("Large-scale structure")
  case object LateTypeStars                extends Keyword("Late-type stars")
  case object LocalGroup                   extends Keyword("Local Group")
  case object LowMassStars                 extends Keyword("Low-mass stars")
  case object LowSurfaceBrightnessGalaxies extends Keyword("Low surface brightness galaxies")
  case object LuminosityFunction           extends Keyword("Luminosity function")
  case object MagellanicCloud              extends Keyword("Magellanic Cloud")
  case object MagneticFields               extends Keyword("Magnetic fields")
  case object Masers                       extends Keyword("Masers")
  case object MassFunction                 extends Keyword("Mass function")
  case object MassiveStars                 extends Keyword("Massive stars ")
  case object Meteoroids                   extends Keyword("Meteoroids")
  case object MilkyWay                     extends Keyword("Milky way")
  case object MinorPlanets                 extends Keyword("Minor planets")
  case object Moon                         extends Keyword("Moon")
  case object MultiMessengerSstrophysics   extends Keyword("Multi-messenger astrophysics")
  case object NeutronStars                 extends Keyword("Neutron stars")
  case object Novae                        extends Keyword("Novae")
  case object Nucleosynthesis              extends Keyword("Nucleosynthesis")
  case object Nuclei                       extends Keyword("Nuclei")
  case object OortCloud                    extends Keyword("Oort cloud")
  case object OpenStarClusters             extends Keyword("Open star clusters")
  case object Oscillations                 extends Keyword("Oscillations")
  case object Outflows                     extends Keyword("Outflows")
  case object PeculiarGalaxies             extends Keyword("Peculiar galaxies")
  case object PlanetaryNebulae             extends Keyword("Planetary nebulae")
  case object PlanetarySatellites          extends Keyword("Planetary satellites")
  case object PlanetarySystems             extends Keyword("Planetary systems")
  case object Polarization                 extends Keyword("Polarization")
  case object PopulationII                 extends Keyword("Population II")
  case object PopulationIII                extends Keyword("Population III")
  case object PreMainSequenceStars         extends Keyword("Pre-main sequence stars")
  case object Pulsars                      extends Keyword("Pulsars")
  case object Quasars                      extends Keyword("Quasars")
  case object RadiationMechanisms          extends Keyword("Radiation mechanisms")
  case object Redshifts                    extends Keyword("Redshifts")
  case object RingsAroundPlanets           extends Keyword("Rings around planets")
  case object SolarNeighborhood            extends Keyword("Solar neighborhood")
  case object SolarSystem                  extends Keyword("Solar system")
  case object SpiralGalaxies               extends Keyword("Spiral galaxies")
  case object StarburstGalaxies            extends Keyword("Starburst galaxies")
  case object StarClusters                 extends Keyword("Star clusters")
  case object StarFormation                extends Keyword("Star formation")
  case object StellarActivity              extends Keyword("Stellar activity")
  case object StellarPopulations           extends Keyword("Stellar populations")
  case object StellarInteriors             extends Keyword("Stellar interiors")
  case object Structure                    extends Keyword("Structure")
  case object Subdwarfs                    extends Keyword("Subdwarfs")
  case object Sun                          extends Keyword("Sun")
  case object Supergiants                  extends Keyword("Supergiants")
  case object Supernovae                   extends Keyword("Supernovae")
  case object Survey                       extends Keyword("Survey")
  case object VariableStars                extends Keyword("Variable stars")
  case object WhiteDwarfs                  extends Keyword("White dwarfs")
  case object WolfRayetStars               extends Keyword("Wolf-Rayet stars")

  implicit val KeywordsEnumerated: Enumerated[Keyword] =
    Enumerated.of(
      AbsorptionLines,
      Accretion,
      ActiveGalaxies,
      AgbStars,
      Asteroids,
      Astrobiology,
      Astrochemistry,
      Astrometry,
      Atmospheres,
      Binaries,
      BlackHolePhysics,
      BlueStragglers,
      BrownDwarfs,
      Bulges,
      CarbonStars,
      CataclysmicVariables,
      ChemicalAbundances,
      CircumstellarMatter,
      Comets,
      CoolingFlows,
      CosmologicalParameters,
      DarkMatter,
      DiffuseFadiation,
      Distances,
      Dust,
      DwarfGalaxies,
      DwarfNovae,
      Dynamics,
      EarlyTypeStars,
      EarlyUniverse,
      Earth,
      EllipticalGalaxies,
      EmissionLines,
      Evolution,
      Extinction,
      Formation,
      FundamentalParameters,
      GalaxyClusters,
      GalacticStructure,
      GammaRayBursts,
      GlobularClusters,
      GravitationalLensing,
      GroupsOfGalaxies,
      HIIRegions,
      Halos,
      HerbigHaroObjects,
      HighRedshift,
      HorizontalBranchStars,
      InteractingGalaxies,
      IntergalacticMedium,
      InterplanetaryMedium,
      InterstellarMedium,
      IrregularGalaxies,
      Jets,
      KuiperBelt,
      LargeScaleStructure,
      LateTypeStars,
      LocalGroup,
      LowMassStars,
      LowSurfaceBrightnessGalaxies,
      LuminosityFunction,
      MagellanicCloud,
      MagneticFields,
      Masers,
      MassFunction,
      MassiveStars,
      Meteoroids,
      MilkyWay,
      MinorPlanets,
      Moon,
      MultiMessengerSstrophysics,
      NeutronStars,
      Novae,
      Nucleosynthesis,
      Nuclei,
      OortCloud,
      OpenStarClusters,
      Oscillations,
      Outflows,
      PeculiarGalaxies,
      PlanetaryNebulae,
      PlanetarySatellites,
      PlanetarySystems,
      Polarization,
      PopulationII,
      PopulationIII,
      PreMainSequenceStars,
      Pulsars,
      Quasars,
      RadiationMechanisms,
      Redshifts,
      RingsAroundPlanets,
      SolarNeighborhood,
      SolarSystem,
      SpiralGalaxies,
      StarburstGalaxies,
      StarClusters,
      StarFormation,
      StellarActivity,
      StellarPopulations,
      StellarInteriors,
      Structure,
      Subdwarfs,
      Sun,
      Supergiants,
      Supernovae,
      Survey,
      VariableStars,
      WhiteDwarfs,
      WolfRayetStars
    )
}
