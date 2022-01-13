// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

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

package object model {
  // It'd be nice to make these opaque
  type TargetWithId            = (Target.Id, Target)
  type SiderealTargetWithId    = (Target.Id, Target.Sidereal)
  type NonsiderealTargetWithId = (Target.Id, Target.Nonsidereal)

  object TargetWithId {
    val sidereal: Prism[TargetWithId, SiderealTargetWithId] =
      Prism.partial[TargetWithId, SiderealTargetWithId] {
        case (id, t @ Target.Sidereal(_, _, _, _, _)) =>
          id -> t
      }(identity)

    val nonsidereal: Prism[TargetWithId, NonsiderealTargetWithId] =
      Prism.partial[TargetWithId, NonsiderealTargetWithId] {
        case (id, t @ Target.Nonsidereal(_, _, _, _)) =>
          id -> t
      }(identity)

    val id: Lens[TargetWithId, Target.Id]                                                        = Focus[TargetWithId](_._1)
    val target: Lens[TargetWithId, Target]                                                       = Focus[TargetWithId](_._2)
    val name: Lens[TargetWithId, NonEmptyString]                                                 = target.andThen(Target.name)
    val integratedBrightnesses
      : Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Integrated]]] =
      target.andThen(Target.integratedBrightnesses)
    val surfaceBrightnesses: Optional[TargetWithId, SortedMap[Band, BrightnessMeasure[Surface]]] =
      target.andThen(Target.surfaceBrightnesses)

    implicit val targetWithIdDecoder: Decoder[TargetWithId] = Decoder.instance(c =>
      for {
        id     <- c.get[Target.Id]("id")
        target <- c.as[Target]
      } yield (id, target)
    )
  }
}
