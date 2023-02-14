// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import cats.syntax.all.*
import explore.model.enums.PosAngleOptions
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.model.PosAngleConstraint

import scala.collection.immutable.SortedMap

object all:

  extension (pa: PosAngleOptions)
    def toPosAngle(a: Angle): PosAngleConstraint = pa match
      case PosAngleOptions.Fixed               => PosAngleConstraint.Fixed(a)
      case PosAngleOptions.AllowFlip           => PosAngleConstraint.AllowFlip(a)
      case PosAngleOptions.AverageParallactic  => PosAngleConstraint.AverageParallactic
      case PosAngleOptions.ParallacticOverride => PosAngleConstraint.ParallacticOverride(a)
      case PosAngleOptions.Unconstrained       => PosAngleConstraint.Unbounded

  extension (pa: PosAngleConstraint)
    def toPosAngleOptions: PosAngleOptions = pa match
      case PosAngleConstraint.Fixed(_)               => PosAngleOptions.Fixed
      case PosAngleConstraint.AllowFlip(_)           => PosAngleOptions.AllowFlip
      case PosAngleConstraint.AverageParallactic     => PosAngleOptions.AverageParallactic
      case PosAngleConstraint.ParallacticOverride(_) => PosAngleOptions.ParallacticOverride
      case PosAngleConstraint.Unbounded              => PosAngleOptions.Unconstrained

  extension [A](list: List[A])
    def toSortedMap[K: Ordering, V](getKey: A => K, getValue: A => V = identity[A](_)) =
      SortedMap.from(list.map(a => (getKey(a), getValue(a))))

  // TODO Move to core
  extension (site: Site)
    def inPreferredDeclination(d: Declination): Boolean =
      d.toAngle.toSignedDoubleDegrees match
        case d if d < -40.0 => site === Site.GS
        case d if d > 30.0  => site === Site.GN
        case _              => true
