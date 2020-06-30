// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.util.UUID

import cats._
import cats.implicits._
import gem.Target
import gsp.math.ProperMotion
import monocle.Lens
import monocle.macros.Lenses
import gsp.math.Coordinates
import gsp.math.RightAscension
import gsp.math.Epoch
import gsp.math.Declination
import cats.effect.Sync

/**
  * A refinement of gem.Tracker meant for sidereal targets
  */
@Lenses
final case class SiderealTarget(id: SiderealTarget.Id, name: String, track: ProperMotion)

object SiderealTarget {
  type Id = UUID

  def New[F[_]: Sync]: F[SiderealTarget] =
    Sync[F]
      .delay(UUID.randomUUID())
      .map(id =>
        SiderealTarget(
          id,
          "New Target",
          ProperMotion(Coordinates(RightAscension.Zero, Declination.Zero),
                       Epoch.J2000,
                       none,
                       none,
                       none
          )
        )
      )

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
      .map(pm =>
        ExploreSiderealTarget(
          target.name,
          SiderealTarget(UUID.randomUUID, target.name, pm).some
        )
      )
      .toOption
}
