// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.implicits._
import gem.Target
import gsp.math.ProperMotion
import monocle.Lens
import monocle.macros.Lenses

/**
  * A refinement of gem.Tracker meant for sidereal targets
  */
@Lenses
final case class SiderealTarget(name: String, track: ProperMotion)

object SiderealTarget        {
  implicit val siderealTargetEq: Eq[SiderealTarget] = Eq.by(x => (x.name, x.track))
}

/**
  * A Sidereal target from a UI point of view containing local state, e.g. catalog selected, etc
  */
@Lenses
final case class ExploreSiderealTarget(searchTerm: String, target: Option[SiderealTarget])

object ExploreSiderealTarget {
  implicit val siderealTargetEq: Eq[ExploreSiderealTarget] = Eq.by(x => (x.searchTerm, x.target))

  def apply(target: Target): Option[ExploreSiderealTarget] =
    target.track
      .map(pm => ExploreSiderealTarget(target.name, SiderealTarget(target.name, pm).some))
      .toOption
}
