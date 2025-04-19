// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.scalacheck.all.given
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.boopickle.PicklerTests
import lucuma.ags.GuideStarCandidate
import lucuma.ags.arb.ArbGuideStarCandidate.given
import lucuma.core.math.*
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.math.arb.ArbDeclination.given
import lucuma.core.math.arb.ArbEpoch.given
import lucuma.core.math.arb.ArbParallax.given
import lucuma.core.math.arb.ArbProperMotion.given
import lucuma.core.math.arb.ArbRadialVelocity.given
import lucuma.core.math.arb.ArbRightAscension.given
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.SiderealTracking
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.arb.ArbElevationRange.given
import lucuma.core.model.arb.ArbSiderealTracking.given
import lucuma.core.model.arb.ArbUnnormalizedSED.given

class BoopickleSuite
    extends munit.DisciplineSuite
    with CatalogPicklers
    with CommonPicklers
    with ItcPicklers {
  checkAll("Pickler[Angle]", PicklerTests[Angle].pickler)
  checkAll("Pickler[HourAngle]", PicklerTests[HourAngle].pickler)
  checkAll("Pickler[RightAscension]", PicklerTests[RightAscension].pickler)
  checkAll("Pickler[NonEmptyString]", PicklerTests[NonEmptyString].pickler)
  checkAll("Pickler[PosInt]", PicklerTests[PosInt].pickler)
  checkAll("Pickler[Declination]", PicklerTests[Declination].pickler)
  checkAll("Pickler[Coordinates]", PicklerTests[Coordinates].pickler)
  checkAll("Pickler[Epoch]", PicklerTests[Epoch].pickler)
  checkAll("Pickler[ProperMotion]", PicklerTests[ProperMotion].pickler)
  checkAll("Pickler[RadialVelocity]", PicklerTests[RadialVelocity].pickler)
  checkAll("Pickler[Parallax]", PicklerTests[Parallax].pickler)
  checkAll("Pickler[SiderealTracking]", PicklerTests[SiderealTracking].pickler)
  checkAll("Pickler[GuideStarCandidate]", PicklerTests[GuideStarCandidate].pickler)
  checkAll("Pickler[ElevationRange]", PicklerTests[ElevationRange].pickler)
  checkAll("Pickler[ConstraintSet]", PicklerTests[ConstraintSet].pickler)
  checkAll("Pickler[Wavelengtth]", PicklerTests[Wavelength].pickler)

  checkAll("Pickler[UnnormalizedSED]", PicklerTests[UnnormalizedSED].pickler)
}
