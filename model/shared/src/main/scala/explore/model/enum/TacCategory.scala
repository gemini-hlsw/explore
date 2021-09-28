// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class TacGroup(val label: String) extends Product with Serializable

object TacGroup {
  case object SolarSystem        extends TacGroup("Solar System")
  case object Exoplanets         extends TacGroup("Exoplanets")
  case object GalacticLocalGroup extends TacGroup("Galactic/Local Group")
  case object Extragalactic      extends TacGroup("Extragalactic")

  implicit val TacGroupEnumerated: Enumerated[TacGroup] =
    Enumerated.of(SolarSystem, Exoplanets, GalacticLocalGroup, Extragalactic)
}

sealed abstract class TacCategory(val label: String, val group: TacGroup)
    extends Product
    with Serializable

object TacCategory {
  import TacGroup._

  case object SmallBodies
      extends TacCategory("Small Bodies: Asteroids, Comets, Moons, Kuiper Belt", SolarSystem)
  case object PlanetaryAtmospheres extends TacCategory("Planetary Atmospheres", SolarSystem)
  case object PlanetarySurfaces    extends TacCategory("Planetary Surfaces", SolarSystem)
  case object SolarSystemOther     extends TacCategory("Solar System Other", SolarSystem)

  case object ExoplanetRadialVelocities
      extends TacCategory("Exoplanet Radial Velocities", Exoplanets)
  case object ExoplanetAtmospheresActivity
      extends TacCategory("Exoplanet Atmospheres/Activity", Exoplanets)
  case object ExoplanetTransits
      extends TacCategory("Exoplanet Transits, Rossiter McLaughlin", Exoplanets)
  case object ExoplanetHostStar
      extends TacCategory("Exoplanet Host Star Properties/Connections", Exoplanets)
  case object ExoplanetOther extends TacCategory("Exoplanet Other", Exoplanets)

  case object StellarAstrophysics
      extends TacCategory("Stellar Astrophysics, Evolution, Supernovae, Abundances",
                          GalacticLocalGroup
      )
  case object StellarPopulations
      extends TacCategory("Stellar Populations, Clusters, Chemical Evolution", GalacticLocalGroup)
  case object StarFormation extends TacCategory("Star Formation", GalacticLocalGroup)
  case object GaseousAstrophysics
      extends TacCategory("Gaseous Astrophysics, H II regions, PN, ISM, SN remnants, Novae",
                          GalacticLocalGroup
      )
  case object StellarRemnants
      extends TacCategory("Stellar Remnants/Compact Objects, WD, NS, BH", GalacticLocalGroup)
  case object GalacticOther extends TacCategory("Galactic Other", GalacticLocalGroup)

  case object Cosmology
      extends TacCategory("Cosmology, Fundamental Physics, Large Scale Structure", Extragalactic)
  case object ClustersOfGalaxies extends TacCategory("Clusters/Groups of Galaxies", Extragalactic)
  case object HighZUniverse      extends TacCategory("High-z Universe", Extragalactic)
  case object LowZUniverse       extends TacCategory("Low-z Universe", Extragalactic)
  case object ActiveGalaxies     extends TacCategory("Active Galaxies, Quasars, SMBH", Extragalactic)
  case object ExtragalacticOther extends TacCategory("Extragalactic Other", Extragalactic)

  implicit val TacCategoryEnumerated: Enumerated[TacCategory] =
    Enumerated.of(
      SmallBodies,
      PlanetaryAtmospheres,
      PlanetarySurfaces,
      SolarSystemOther,
      ExoplanetRadialVelocities,
      ExoplanetAtmospheresActivity,
      ExoplanetTransits,
      ExoplanetHostStar,
      ExoplanetOther,
      StellarAstrophysics,
      StellarPopulations,
      StarFormation,
      GaseousAstrophysics,
      StellarRemnants,
      GalacticOther,
      Cosmology,
      ClustersOfGalaxies,
      HighZUniverse,
      LowZUniverse,
      ActiveGalaxies,
      ExtragalacticOther
    )
}
