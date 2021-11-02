// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.Order._
import cats.data.NonEmptySet
import cats.syntax.all._
import explore.model.util.NonEmptySetWrapper
import lucuma.core.model.Observation
import lucuma.core.model.TargetEnvironment
import lucuma.core.optics.Format
import monocle.Iso

import scala.collection.immutable.SortedSet

case class TargetEnvGroupIdSet(idSet: NonEmptySet[TargetEnvGroupId]) {
  lazy val targetEnvIds: TargetEnvIdSet      = TargetEnvIdSet(idSet.map(_.targetEnvId))
  lazy val obsIds: SortedSet[Observation.Id] = SortedSet.from(idSet.toList.mapFilter(_.optObsId))
  lazy val obsIdList: List[Observation.Id]   = obsIds.toList

  lazy val unmooredTargetEnvIds: SortedSet[TargetEnvironment.Id] =
    idSet.collect { case id if id.optObsId.isEmpty => id.targetEnvId }

  override def toString: String = idSet.toSortedSet
    .map(_.toString)
    .mkString(",")

  def firstAndOnlyObsId: Option[Observation.Id] =
    if (idSet.length === 1) idSet.head.optObsId else none
}

object TargetEnvGroupIdSet {
  implicit val orderTargetEnvGroupIdSet: Order[TargetEnvGroupIdSet] = Order.by(_.idSet)

  val iso: Iso[TargetEnvGroupIdSet, NonEmptySet[TargetEnvGroupId]] =
    Iso[TargetEnvGroupIdSet, NonEmptySet[TargetEnvGroupId]](_.idSet)(TargetEnvGroupIdSet.apply)

  implicit def nonEmptySetWrapper(ids: TargetEnvGroupIdSet) =
    NonEmptySetWrapper(ids, TargetEnvGroupIdSet.iso)

  // alternate ordering for display
  val orderByObsThenTargetEnv: Order[TargetEnvGroupIdSet] =
    Order.by(_.idSet.toList.map(t => (t.optObsId, t.targetEnvId)))

  val format: Format[String, TargetEnvGroupIdSet] = Format(parse, _.toString)

  private def parse(idSetStr: String): Option[TargetEnvGroupIdSet] =
    idSetStr
      .split(",")
      .toList
      .traverse(TargetEnvGroupId.format.getOption)
      .flatMap(list => NonEmptySet.fromSet(SortedSet.from(list)).map(TargetEnvGroupIdSet.apply))

  def one(id: TargetEnvGroupId): TargetEnvGroupIdSet = TargetEnvGroupIdSet(NonEmptySet.one(id))
}
