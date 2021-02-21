// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class TacCategory(val label: String) extends Product with Serializable

object TacCategory {
  case object PlanetarySystems         extends TacCategory("Planetary systems")
  case object StarAndPlanetFormation   extends TacCategory("Star and planet formation")
  case object StarsAndStellarEvolution extends TacCategory("Stars and stellar evolution")
  case object FormationOfCompactObjects
      extends TacCategory("Formation and evolution of compact objects")
  case object ResolvedStellarPopulations
      extends TacCategory("Resoloved stellar populations and their environments")
  case object GalaxyEvolution          extends TacCategory("Galaxy evolution")
  case object CosmologyAndFundamentalPhysics
      extends TacCategory("Cosmology and fundamental physics")

  implicit val TacCategoryEnumerated: Enumerated[TacCategory] =
    Enumerated.of(
      PlanetarySystems,
      StarAndPlanetFormation,
      StarsAndStellarEvolution,
      FormationOfCompactObjects,
      ResolvedStellarPopulations,
      GalaxyEvolution,
      CosmologyAndFundamentalPhysics
    )
}
