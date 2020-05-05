// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.implicits._
import gem.Target
import monocle.Lens
import monocle.macros.Lenses
import gsp.math.ProperMotion

@Lenses
final case class SiderealTarget(name: String, track: ProperMotion)

object SiderealTarget        {
  implicit val siderealTargetEq: Eq[SiderealTarget] = Eq.by(x => (x.name, x.track))
}

@Lenses
final case class ExploreSiderealTarget(searchTerm: String, target: Option[SiderealTarget])

object ExploreSiderealTarget {

  def apply(target: Target): Option[ExploreSiderealTarget] =
    target.track
      .map(pm => ExploreSiderealTarget(target.name, SiderealTarget(target.name, pm).some))
      .toOption

  val searchTermL: Lens[Option[ExploreSiderealTarget], String] =
    Lens[Option[ExploreSiderealTarget], String](_.map(_.searchTerm).orEmpty)(b =>
      s => s.map(_.copy(searchTerm = b))
    )
}
