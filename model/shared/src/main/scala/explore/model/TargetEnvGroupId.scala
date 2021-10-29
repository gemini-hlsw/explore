// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.syntax.all._
import lucuma.core.model.Observation
import lucuma.core.model.TargetEnvironment
import lucuma.core.optics.Format

case class TargetEnvGroupId(id: (TargetEnvironment.Id, Option[Observation.Id])) extends AnyVal {
  def targetEnvId: TargetEnvironment.Id = id._1
  def optObsId: Option[Observation.Id]  = id._2

  override def toString: String = {
    // none can be anything, as long as it doesn't parse into an obs id
    val obsIdStr = id._2.fold("none")(_.toString)
    s"${id._1}:$obsIdStr"
  }
}

object TargetEnvGroupId {
  implicit val orderTargetEnvGroupId: Order[TargetEnvGroupId] = Order.by(_.id._1)

  val format: Format[String, TargetEnvGroupId] = Format(parse, _.toString)

  def parse(idStr: String): Option[TargetEnvGroupId] =
    idStr.split(":").toList match {
      case tidStr :: oidStr :: Nil =>
        TargetEnvironment.Id
          .parse(tidStr)
          .map(tid => TargetEnvGroupId((tid, Observation.Id.parse(oidStr))))
      case _                       => none
    }

}
