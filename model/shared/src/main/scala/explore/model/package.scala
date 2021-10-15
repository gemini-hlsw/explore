// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.data.NonEmptySet
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.model.EphemerisKey
import lucuma.core.model.Magnitude
import lucuma.core.model.NonsiderealTarget
import lucuma.core.model.SiderealTarget
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism

import scala.collection.immutable.SortedMap

package object model {
  // It'd be nice to make these opaque
  type TargetIdSet             = NonEmptySet[Target.Id]
  type TargetWithId            = (TargetIdSet, Target)
  type SiderealTargetWithId    = (TargetIdSet, SiderealTarget)
  type NonsiderealTargetWithId = (TargetIdSet, NonsiderealTarget)

  object TargetIdSet {
    def fromTargetIdList(targetIds: List[Target.Id]): Option[TargetIdSet] =
      targetIds match {
        case Nil          => none
        case head :: tail => NonEmptySet.of(head, tail: _*).some
      }
  }

  object SiderealTargetWithId {
    val id: Lens[SiderealTargetWithId, TargetIdSet]                                 = Focus[SiderealTargetWithId](_._1)
    val target: Lens[SiderealTargetWithId, SiderealTarget]                          = Focus[SiderealTargetWithId](_._2)
    val name: Lens[SiderealTargetWithId, NonEmptyString]                            = target.andThen(SiderealTarget.name)
    val magnitudes: Lens[SiderealTargetWithId, SortedMap[MagnitudeBand, Magnitude]] =
      target.andThen(SiderealTarget.magnitudes)

    val tracking: Lens[SiderealTargetWithId, SiderealTracking] =
      target.andThen(SiderealTarget.tracking)

    val baseRA: Lens[SiderealTargetWithId, RightAscension]                 =
      target.andThen(SiderealTarget.baseRA)
    val baseDec: Lens[SiderealTargetWithId, Declination]                   =
      target.andThen(SiderealTarget.baseDec)
    val epoch: Lens[SiderealTargetWithId, Epoch]                           =
      target.andThen(SiderealTarget.epoch)
    val properMotionRA: Optional[SiderealTargetWithId, ProperMotion.RA]    =
      target.andThen(SiderealTarget.properMotionRA)
    val properMotionDec: Optional[SiderealTargetWithId, ProperMotion.Dec]  =
      target.andThen(SiderealTarget.properMotionDec)
    val radialVelocity: Lens[SiderealTargetWithId, Option[RadialVelocity]] =
      target.andThen(SiderealTarget.radialVelocity)
    val parallax: Lens[SiderealTargetWithId, Option[Parallax]]             =
      target.andThen(SiderealTarget.parallax)
  }

  object NonsiderealTargetWithId {
    val id: Lens[NonsiderealTargetWithId, TargetIdSet]                                 = Focus[NonsiderealTargetWithId](_._1)
    val target: Lens[NonsiderealTargetWithId, NonsiderealTarget]                       =
      Focus[NonsiderealTargetWithId](_._2)
    val name: Lens[NonsiderealTargetWithId, NonEmptyString]                            = target.andThen(NonsiderealTarget.name)
    val magnitudes: Lens[NonsiderealTargetWithId, SortedMap[MagnitudeBand, Magnitude]] =
      target.andThen(NonsiderealTarget.magnitudes)

    val ephemerisKey: Lens[NonsiderealTargetWithId, EphemerisKey] =
      target.andThen(NonsiderealTarget.ephemerisKey)
  }

  object TargetWithId {
    val sidereal: Prism[TargetWithId, SiderealTargetWithId] =
      Prism.partial[TargetWithId, SiderealTargetWithId] { case (id, t @ SiderealTarget(_, _, _)) =>
        id -> t
      }(identity)

    val nonsidereal: Prism[TargetWithId, NonsiderealTargetWithId] =
      Prism.partial[TargetWithId, NonsiderealTargetWithId] {
        case (id, t @ NonsiderealTarget(_, _, _)) =>
          id -> t
      }(identity)

    val id: Lens[TargetWithId, TargetIdSet]                                 = Focus[TargetWithId](_._1)
    val target: Lens[TargetWithId, Target]                                  = Focus[TargetWithId](_._2)
    val name: Lens[TargetWithId, NonEmptyString]                            = target.andThen(Target.name)
    val magnitudes: Lens[TargetWithId, SortedMap[MagnitudeBand, Magnitude]] =
      target.andThen(Target.magnitudes)
  }
}
