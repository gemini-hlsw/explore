// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Decoder._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.model.Target
import lucuma.schemas.decoders._
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism

import scala.collection.immutable.SortedMap

case class TargetWithId(id: Target.Id, target: Target) {
  def toOptId: TargetWithOptId                       = TargetWithOptId(id.some, target)
  def toSidereal: Option[SiderealTargetWithId]       = TargetWithId.sidereal.getOption(this)
  def toNonSidereal: Option[NonsiderealTargetWithId] = TargetWithId.nonsidereal.getOption(this)
}

case class TargetWithOptId(optId: Option[Target.Id], target: Target)

case class SiderealTargetWithId(id: Target.Id, target: Target.Sidereal) {
  def toTargetWithId = TargetWithId(id, target)
}

case class NonsiderealTargetWithId(id: Target.Id, target: Target.Nonsidereal) {
  def toTargetWithId = TargetWithId(id, target)
}

object TargetWithId {
  val id: Lens[TargetWithId, Target.Id]        = Focus[TargetWithId](_.id)
  val target: Lens[TargetWithId, Target]       = Focus[TargetWithId](_.target)
  val name: Lens[TargetWithId, NonEmptyString] = target.andThen(Target.name)

  val integratedBrightnesses
    : Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Integrated]]] =
    target.andThen(Target.integratedBrightnesses)
  val surfaceBrightnesses: Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Surface]]] =
    target.andThen(Target.surfaceBrightnesses)

  val sidereal: Prism[TargetWithId, SiderealTargetWithId] =
    Prism.partial[TargetWithId, SiderealTargetWithId] {
      case TargetWithId(id, t @ Target.Sidereal(_, _, _, _, _)) =>
        SiderealTargetWithId(id, t)
    }(_.toTargetWithId)

  val nonsidereal: Prism[TargetWithId, NonsiderealTargetWithId] =
    Prism.partial[TargetWithId, NonsiderealTargetWithId] {
      case TargetWithId(id, t @ Target.Nonsidereal(_, _, _, _)) =>
        NonsiderealTargetWithId(id, t)
    }(_.toTargetWithId)

  implicit val targetWithIdDecoder: Decoder[TargetWithId] = Decoder.instance(c =>
    for {
      id     <- c.get[Target.Id]("id")
      target <- c.as[Target]
    } yield TargetWithId(id, target)
  )

  implicit val eqTargetWithId: Eq[TargetWithId] = Eq.by(x => (x.id, x.target))
}

object TargetWithOptId {
  implicit val eqTargetWithOptId: Eq[TargetWithOptId] = Eq.by(x => (x.optId, x.target))
}
