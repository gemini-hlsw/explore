// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.model.decoders._
import io.circe.Decoder
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import monocle.Lens
import monocle.Focus
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.MagnitudeBand
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTarget
import lucuma.core.math.RightAscension
import scala.collection.immutable.SortedMap
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import monocle.Optional
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Parallax
import scala.collection.immutable.TreeSeqMap

case class TargetEnv(
  id:             TargetEnvironment.Id,
  scienceTargets: TreeSeqMap[Target.Id, ScienceTarget[Target]]
)

object TargetEnv {
  implicit val eqTargetEnv: Eq[TargetEnv] = Eq.by(x => (x.id, x.scienceTargets.toMap))

  implicit val decoderTargetEnv: Decoder[TargetEnv] = Decoder.instance(c =>
    for {
      id             <- c.downField("id").as[TargetEnvironment.Id]
      scienceTargets <-
        c.downField("scienceTargets")
          .as[List[ScienceTarget[Target]]]
          .map(list => TreeSeqMap(list.map(t => t.id -> t): _*))
    } yield TargetEnv(id, scienceTargets)
  )

  val id: Lens[TargetEnv, TargetEnvironment.Id]                                     = Focus[TargetEnv](_.id)
  val scienceTargets: Lens[TargetEnv, TreeSeqMap[Target.Id, ScienceTarget[Target]]] =
    Focus[TargetEnv](_.scienceTargets)
}

/**
 * Target.Id + Target
 */
case class ScienceTarget[+T](id: Target.Id, target: T)
object ScienceTarget {
  implicit val eqScienceTarget: Eq[ScienceTarget[Target]] = Eq.by(x => (x.id, x.target))

  implicit val decoderTargetEnv: Decoder[ScienceTarget[Target]] = Decoder.instance(c =>
    for {
      id     <- c.downField("id").as[Target.Id]
      target <- c.as[Target]
    } yield ScienceTarget(id, target)
  )

  val id: Lens[ScienceTarget[Target], Target.Id]                                   = Focus[ScienceTarget[Target]](_.id)
  def target[T]: Lens[ScienceTarget[T], T]                                         = Focus[ScienceTarget[T]](_.target)
  val name: Lens[ScienceTarget[Target], NonEmptyString]                            = target.andThen(Target.name)
  val magnitudes: Lens[ScienceTarget[Target], SortedMap[MagnitudeBand, Magnitude]] =
    target.andThen(Target.magnitudes)
  object Sidereal {
    val baseRA: Lens[ScienceTarget[SiderealTarget], RightAscension]                 =
      target.andThen(SiderealTarget.baseRA)
    val baseDec: Lens[ScienceTarget[SiderealTarget], Declination]                   =
      target.andThen(SiderealTarget.baseDec)
    val epoch: Lens[ScienceTarget[SiderealTarget], Epoch]                           =
      target.andThen(SiderealTarget.epoch)
    val properMotionRA: Optional[ScienceTarget[SiderealTarget], ProperMotion.RA]    =
      target.andThen(SiderealTarget.properMotionRA)
    val properMotionDec: Optional[ScienceTarget[SiderealTarget], ProperMotion.Dec]  =
      target.andThen(SiderealTarget.properMotionDec)
    val radialVelocity: Lens[ScienceTarget[SiderealTarget], Option[RadialVelocity]] =
      target.andThen(SiderealTarget.radialVelocity)
    val parallax: Lens[ScienceTarget[SiderealTarget], Option[Parallax]]             =
      target.andThen(SiderealTarget.parallax)
  }
}
