// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.Order.*
import cats.Semigroup
import cats.Show
import cats.data.NonEmptySet
import cats.syntax.all.*
import explore.model.util.NonEmptySetWrapper
import monocle.Iso
import monocle.Prism

import scala.collection.immutable.SortedSet

case class ObsIdSet(idSet: NonEmptySet[Observation.Id]):
  def ++(other: ObsIdSet): ObsIdSet = ObsIdSet(idSet ++ other.idSet)
  def --(other: Set[Observation.Id]): Option[ObsIdSet] =
    NonEmptySet.fromSet(idSet.toSortedSet -- other).map(ObsIdSet(_))
  def --(other: ObsIdSet): Option[ObsIdSet]            =
    --(other.idSet.toSortedSet)
  def -(other: Observation.Id): Option[ObsIdSet]       =
    NonEmptySet.fromSet(idSet - other).map(ObsIdSet(_))

object ObsIdSet {
  given Order[ObsIdSet] = Order.by(_.idSet)

  given Show[ObsIdSet] = Show.show(_.idSet.toList.mkString(", "))

  given Semigroup[ObsIdSet] = Semigroup.instance((a, b) => ObsIdSet(a.idSet |+| b.idSet))

  val iso: Iso[ObsIdSet, NonEmptySet[Observation.Id]] =
    Iso[ObsIdSet, NonEmptySet[Observation.Id]](_.idSet)(ObsIdSet.apply)

  implicit def nonEmptySetWrapper(ids: ObsIdSet): NonEmptySetWrapper[ObsIdSet, Observation.Id] =
    NonEmptySetWrapper(ids, iso)

  def fromSortedSet(obsIds: SortedSet[Observation.Id]): Option[ObsIdSet] =
    NonEmptySet.fromSet(obsIds).map(ObsIdSet.apply)

  def fromList(obsIds: List[Observation.Id]): Option[ObsIdSet] =
    fromSortedSet(SortedSet.from(obsIds))

  val fromString: Prism[String, ObsIdSet] =
    Prism(parse)(ids =>
      eu.timepit.refined.types.string.NonEmptyString
        .unsafeFrom(ids.idSet.toSortedSet.map(_.toString).mkString(","))
        .value
    )

  private def parse(idSetStr: String): Option[ObsIdSet] =
    idSetStr
      .split(",")
      .toList
      .traverse(Observation.Id.parse)
      .flatMap(fromList)

  def one(id: Observation.Id): ObsIdSet = ObsIdSet(NonEmptySet.one(id))

  def of(id: Observation.Id, ids: Observation.Id*): ObsIdSet = ObsIdSet(NonEmptySet.of(id, ids*))
}
