// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.functor._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.decoders._
import io.circe.Decoder
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
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.SortedMap

/**
 * Target.Id + Target
 */
sealed trait ScienceTarget {
  val id: Target.Id
  val target: Target
}

final case class SiderealScienceTarget(id: Target.Id, target: SiderealTarget) extends ScienceTarget
object SiderealScienceTarget {
  implicit val decoderSiderealTargetEnv: Decoder[SiderealScienceTarget] = Decoder.instance(c =>
    for {
      id     <- c.downField("id").as[Target.Id]
      target <- c.as[SiderealTarget]
    } yield SiderealScienceTarget(id, target)
  )

  val id: Lens[SiderealScienceTarget, Target.Id]                                   = Focus[SiderealScienceTarget](_.id)
  val target: Lens[SiderealScienceTarget, SiderealTarget]                          = Focus[SiderealScienceTarget](_.target)
  val name: Lens[SiderealScienceTarget, NonEmptyString]                            = target.andThen(SiderealTarget.name)
  val magnitudes: Lens[SiderealScienceTarget, SortedMap[MagnitudeBand, Magnitude]] =
    target.andThen(SiderealTarget.magnitudes)

  val baseRA: Lens[SiderealScienceTarget, RightAscension]                 =
    target.andThen(SiderealTarget.baseRA)
  val baseDec: Lens[SiderealScienceTarget, Declination]                   =
    target.andThen(SiderealTarget.baseDec)
  val epoch: Lens[SiderealScienceTarget, Epoch]                           =
    target.andThen(SiderealTarget.epoch)
  val properMotionRA: Optional[SiderealScienceTarget, ProperMotion.RA]    =
    target.andThen(SiderealTarget.properMotionRA)
  val properMotionDec: Optional[SiderealScienceTarget, ProperMotion.Dec]  =
    target.andThen(SiderealTarget.properMotionDec)
  val radialVelocity: Lens[SiderealScienceTarget, Option[RadialVelocity]] =
    target.andThen(SiderealTarget.radialVelocity)
  val parallax: Lens[SiderealScienceTarget, Option[Parallax]]             =
    target.andThen(SiderealTarget.parallax)
}

final case class NonsiderealScienceTarget(id: Target.Id, target: NonsiderealTarget)
    extends ScienceTarget

object NonsiderealScienceTarget {
  implicit val decoderNonsiderealTargetEnv: Decoder[NonsiderealScienceTarget] =
    Decoder.instance(c =>
      for {
        id     <- c.downField("id").as[Target.Id]
        target <- c.as[NonsiderealTarget]
      } yield NonsiderealScienceTarget(id, target)
    )

  val id: Lens[NonsiderealScienceTarget, Target.Id]                                   = Focus[NonsiderealScienceTarget](_.id)
  val target: Lens[NonsiderealScienceTarget, NonsiderealTarget]                       =
    Focus[NonsiderealScienceTarget](_.target)
  val name: Lens[NonsiderealScienceTarget, NonEmptyString]                            = target.andThen(NonsiderealTarget.name)
  val magnitudes: Lens[NonsiderealScienceTarget, SortedMap[MagnitudeBand, Magnitude]] =
    target.andThen(NonsiderealTarget.magnitudes)

  val ephemerisKey: Lens[NonsiderealScienceTarget, EphemerisKey] =
    target.andThen(NonsiderealTarget.ephemerisKey)
}

object ScienceTarget {
  implicit val eqScienceTarget: Eq[ScienceTarget] = Eq.by(x => (x.id, x.target))

  implicit val decoderTargetEnv: Decoder[ScienceTarget] =
    List[Decoder[ScienceTarget]](Decoder[SiderealScienceTarget].widen,
                                 Decoder[NonsiderealScienceTarget].widen
    ).reduceLeft(_ or _)

  val id: Lens[ScienceTarget, Target.Id] =
    Lens[ScienceTarget, Target.Id](_.id)(v => {
      case t @ SiderealScienceTarget(_, _)    => SiderealScienceTarget.id.replace(v)(t)
      case t @ NonsiderealScienceTarget(_, _) => NonsiderealScienceTarget.id.replace(v)(t)
    })

  val name: Lens[ScienceTarget, NonEmptyString] =
    Lens[ScienceTarget, NonEmptyString](_.target.name)(v => {
      case t @ SiderealScienceTarget(_, _)    => SiderealScienceTarget.name.replace(v)(t)
      case t @ NonsiderealScienceTarget(_, _) => NonsiderealScienceTarget.name.replace(v)(t)
    })

  val magnitudes: Lens[ScienceTarget, SortedMap[MagnitudeBand, Magnitude]] =
    Lens[ScienceTarget, SortedMap[MagnitudeBand, Magnitude]](_.target.magnitudes)(v => {
      case t @ SiderealScienceTarget(_, _)    => SiderealScienceTarget.magnitudes.replace(v)(t)
      case t @ NonsiderealScienceTarget(_, _) => NonsiderealScienceTarget.magnitudes.replace(v)(t)
    })
}
