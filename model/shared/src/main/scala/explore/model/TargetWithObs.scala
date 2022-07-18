// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.implicits._
import io.circe.Decoder
import io.circe.Decoder._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

// We keep track of the observations a target is in to know if it's shared or not.
final case class TargetWithObs(target: Target, obsIds: SortedSet[Observation.Id]) {
  def addObsIds(ids: ObsIdSet): TargetWithObs =
    TargetWithObs.obsIds.modify(_ ++ ids.toSortedSet)(this)

  def removeObsIds(ids: ObsIdSet): TargetWithObs =
    TargetWithObs.obsIds.modify(_ -- ids.toSortedSet)(this)
}

object TargetWithObs {
  implicit val eqTargetWithObs: Eq[TargetWithObs] = Eq.by(x => (x.target, x.obsIds))

  val target: Lens[TargetWithObs, Target] =
    Focus[TargetWithObs](_.target)

  val obsIds: Lens[TargetWithObs, SortedSet[Observation.Id]] =
    Focus[TargetWithObs](_.obsIds)
}

final case class TargetWithIdAndObs(id: Target.Id, targetWithObs: TargetWithObs) {
  def target: Target                    = targetWithObs.target
  def obsIds: SortedSet[Observation.Id] = targetWithObs.obsIds

  def toTargetWithId: TargetWithId = TargetWithId(id, targetWithObs.target)

  def addObsIds(ids: ObsIdSet): TargetWithIdAndObs =
    TargetWithIdAndObs.targetWithObs.modify(_.addObsIds(ids))(this)

  def removeObsIds(ids: ObsIdSet): TargetWithIdAndObs =
    TargetWithIdAndObs.targetWithObs.modify(_.removeObsIds(ids))(this)
}

object TargetWithIdAndObs {
  implicit val eqTargetWithIdAndObs: Eq[TargetWithIdAndObs] = Eq.by(x => (x.id, x.targetWithObs))

  val id: Lens[TargetWithIdAndObs, Target.Id] =
    Focus[TargetWithIdAndObs](_.id)

  val targetWithObs: Lens[TargetWithIdAndObs, TargetWithObs] =
    Focus[TargetWithIdAndObs](_.targetWithObs)

  implicit val targetGroupDecode: Decoder[TargetWithIdAndObs] = Decoder.instance(c =>
    for {
      obsIds       <- c.get[List[Observation.Id]]("observationIds")
      targetWithId <- c.get[TargetWithId]("target")
    } yield TargetWithIdAndObs(
      targetWithId.id,
      TargetWithObs(targetWithId.target, SortedSet.from(obsIds))
    )
  )
}
