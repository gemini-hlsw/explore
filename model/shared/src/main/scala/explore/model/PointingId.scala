// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import lucuma.core.model.Asterism
import lucuma.core.model.Target

// This can probably be turned into an "or type" in Scala 3.
sealed trait PointingId
object PointingId {
  case class TargetId(id: Target.Id) extends PointingId
  object TargetId {
    implicit val eqTargetId: Eq[TargetId] = Eq.by(_.id)
  }

  case class AsterismId(id: Asterism.Id) extends PointingId
  object AsterismId {
    implicit val eqAsterismId: Eq[AsterismId] = Eq.by(_.id)
  }

  implicit val eqPointingId: Eq[PointingId] = Eq.instance((a, b) =>
    (a, b) match {
      case (a @ TargetId(_), b @ TargetId(_))     => a === b
      case (a @ AsterismId(_), b @ AsterismId(_)) => a === b
      case _                                      => false
    }
  )
}
