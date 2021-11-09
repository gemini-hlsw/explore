// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.model.Observation
import monocle.Focus

import scala.annotation.unused

final case class ConstraintGroup(constraintSet: ConstraintSet, obsIds: ObsIdSet) {
  def addObsId(obsId: Observation.Id): ConstraintGroup =
    ConstraintGroup.obsIds.modify(_.add(obsId))(this)

  def removeObsIds(toRemove: ObsIdSet): Option[ConstraintGroup] =
    obsIds.remove(toRemove).map(ids => ConstraintGroup.obsIds.replace(ids)(this))

  def addObsIds(toAdd: ObsIdSet): ConstraintGroup =
    ConstraintGroup.obsIds.modify(_ ++ toAdd)(this)

  def asKeyValue: (ObsIdSet, ConstraintGroup) = (this.obsIds, this)
}

object ConstraintGroup {
  val constraintSet = Focus[ConstraintGroup](_.constraintSet)
  val obsIds        = Focus[ConstraintGroup](_.obsIds)

  private case class ObsNode(id: Observation.Id)
  @unused("used but compiler can't figure it out")
  private implicit val obsNodeDecoder: Decoder[ObsNode] = deriveDecoder

  private case class ObsIdNodes(nodes: List[ObsNode])
  private implicit val obsIdNodesDecoder: Decoder[ObsIdNodes] = deriveDecoder

  implicit val constraintGroupDecoder: Decoder[ConstraintGroup] =
    Decoder.instance(c =>
      for {
        cs     <- c.downField("constraintSet").as[ConstraintSet]
        obsIds <- c.downField("observations").as[ObsIdNodes].map { o =>
                    val ids = o.nodes.map(_.id)
                    ObsIdSet.of(ids.head, ids.tail: _*)
                  }
      } yield ConstraintGroup(cs, obsIds)
    )

  implicit val constraintGroupEq: Eq[ConstraintGroup] = Eq.by(cg => (cg.constraintSet, cg.obsIds))
}
