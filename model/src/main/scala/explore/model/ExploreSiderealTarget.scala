// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.util.UUID

import cats._
import cats.effect.Sync
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import monocle.macros.Lenses

/**
 * A refinement of gem.Tracker meant for sidereal targets
 */
@Lenses
final case class SiderealTarget(
  id:    SiderealTarget.Id,
  name:  NonEmptyString,
  track: SiderealTracking
)

object SiderealTarget {
  type Id = UUID

  def createNew[F[_]: Sync]: F[SiderealTarget] =
    Sync[F]
      .delay(UUID.randomUUID())
      .map(id =>
        SiderealTarget(
          id,
          refineMV[NonEmpty]("New Target"),
          SiderealTracking(none, Coordinates.Zero, Epoch.J2000, none, none, none)
        )
      )

  implicit val siderealTargetEq: Eq[SiderealTarget] = Eq.by(x => (x.name, x.track))

  val baseCoordinates = SiderealTarget.track ^|-> SiderealTracking.baseCoordinates

}

/**
 * A Sidereal target from a UI point of view containing local state, e.g. catalog selected, etc
 */
@Lenses
final case class ExploreSiderealTarget(
  searchTerm: String,
  target:     Option[SiderealTarget],
  options:    TargetVisualOptions
)

object ExploreSiderealTarget {
  implicit val siderealTargetEq: Eq[ExploreSiderealTarget] =
    Eq.by(x => (x.searchTerm, x.target, x.options))

  def apply(target: Target): Option[ExploreSiderealTarget] =
    target.track
      .map(pm =>
        ExploreSiderealTarget(
          target.name.value,
          SiderealTarget(UUID.randomUUID, target.name, pm).some,
          TargetVisualOptions.Default
        )
      )
      .toOption
}
