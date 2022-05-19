// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import explore.boopickle.PicklerTests
import explore.model.CatalogPicklers
import explore.model.CatalogResults
import explore.model.GuideStarCandidate
import explore.model.arb.ArbCatalogResults._
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.HourAngle
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.arb.ArbCoordinates._
import lucuma.core.math.arb.ArbDeclination._
import lucuma.core.math.arb.ArbEpoch._
import lucuma.core.math.arb.ArbParallax._
import lucuma.core.math.arb.ArbProperMotion._
import lucuma.core.math.arb.ArbRadialVelocity._
import lucuma.core.math.arb.ArbRightAscension._
import lucuma.core.model.SiderealTracking
import lucuma.core.model.arb.ArbSiderealTracking._

class BoopickleSuite extends munit.DisciplineSuite with CatalogPicklers {
  checkAll("Pickler[Angle]", PicklerTests[Angle].pickler)
  checkAll("Pickler[HourAngle]", PicklerTests[HourAngle].pickler)
  checkAll("Pickler[RightAscension]", PicklerTests[RightAscension].pickler)
  checkAll("Pickler[NonEmptyString]", PicklerTests[NonEmptyString].pickler)
  checkAll("Pickler[Declination]", PicklerTests[Declination].pickler)
  checkAll("Pickler[Coordinates]", PicklerTests[Coordinates].pickler)
  checkAll("Pickler[Epoch]", PicklerTests[Epoch].pickler)
  checkAll("Pickler[ProperMotion]", PicklerTests[ProperMotion].pickler)
  checkAll("Pickler[RadialVelocity]", PicklerTests[RadialVelocity].pickler)
  checkAll("Pickler[Parallax]", PicklerTests[Parallax].pickler)
  checkAll("Pickler[SiderealTracking]", PicklerTests[SiderealTracking].pickler)
  checkAll("Pickler[GuideStarCandidate]", PicklerTests[GuideStarCandidate].pickler)
  checkAll("Pickler[CatalogResults]", PicklerTests[CatalogResults].pickler)
}
