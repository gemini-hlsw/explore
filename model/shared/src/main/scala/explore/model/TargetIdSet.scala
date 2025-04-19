// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.data.NonEmptySet
import cats.syntax.all.*
import explore.model.util.NonEmptySetWrapper
import lucuma.core.model.Target
import monocle.Iso
import monocle.Prism

case class TargetIdSet(idSet: NonEmptySet[Target.Id])

object TargetIdSet {
  implicit val orderTargetIdSet: Order[TargetIdSet] = Order.by(_.idSet)

  val iso: Iso[TargetIdSet, NonEmptySet[Target.Id]] =
    Iso[TargetIdSet, NonEmptySet[Target.Id]](_.idSet)(TargetIdSet.apply)

  implicit def nonEmptySetWrapper(ids: TargetIdSet): NonEmptySetWrapper[TargetIdSet, Target.Id] =
    NonEmptySetWrapper(ids, iso)

  def fromTargetIdList(targetIds: List[Target.Id]): Option[TargetIdSet] =
    targetIds match {
      case Nil          => none
      case head :: tail => TargetIdSet(NonEmptySet.of(head, tail*)).some
    }

  val fromString: Prism[String, TargetIdSet] =
    Prism(parse)(_.idSet.toSortedSet.map(_.toString).mkString(","))

  def one(id: Target.Id): TargetIdSet = TargetIdSet(NonEmptySet.one(id))

  def of(id: Target.Id, ids: Target.Id*): TargetIdSet = TargetIdSet(NonEmptySet.of(id, ids*))

  private def parse(idSetStr: String): Option[TargetIdSet] =
    idSetStr.split(",").toList.traverse(Target.Id.parse).flatMap(fromTargetIdList)
}
