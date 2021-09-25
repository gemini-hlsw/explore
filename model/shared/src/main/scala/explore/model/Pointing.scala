// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Asterism
import lucuma.core.model.Target

sealed trait Pointing
object Pointing {

  case class PointingTarget(id: Target.Id, name: NonEmptyString) extends Pointing
  object PointingTarget   {
    implicit val eqPointingTarget: Eq[PointingTarget] = Eq.by(t => (t.id, t.name))
  }

  case class PointingAsterism(
    id:      Asterism.Id,
    name:    Option[NonEmptyString],
    targets: List[PointingTarget]
  ) extends Pointing
  object PointingAsterism {
    implicit val eqPointingAsterism: Eq[PointingAsterism] = Eq.by(a => (a.id, a.name, a.targets))
  }

  implicit val eqPointing: Eq[Pointing] = Eq.instance((a, b) =>
    (a, b) match {
      case (a @ PointingTarget(_, _), b @ PointingTarget(_, _))           => a === b
      case (a @ PointingAsterism(_, _, _), b @ PointingAsterism(_, _, _)) => a === b
      case _                                                              => false
    }
  )
}
