// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.Order._
import cats.data.NonEmptySet
import cats.syntax.all._
import explore.model.util.NonEmptySetWrapper
import lucuma.core.model.Observation
import monocle.Iso
import monocle.Prism

import scala.collection.immutable.SortedSet

final case class ObsIdSet(idSet: NonEmptySet[Observation.Id])

object ObsIdSet {
  implicit val orderObsIdSet: Order[ObsIdSet] = Order.by(_.idSet)

  val iso: Iso[ObsIdSet, NonEmptySet[Observation.Id]] =
    Iso[ObsIdSet, NonEmptySet[Observation.Id]](_.idSet)(ObsIdSet.apply)

  implicit def nonEmptySetWrapper(ids: ObsIdSet) = NonEmptySetWrapper(ids, iso)

  val fromString: Prism[String, ObsIdSet] =
    Prism(parse)(_.idSet.toSortedSet.map(_.toString).mkString(","))

  private def parse(idSetStr: String): Option[ObsIdSet] =
    idSetStr
      .split(",")
      .toList
      .traverse(Observation.Id.parse)
      .flatMap(list => NonEmptySet.fromSet(SortedSet.from(list)).map(ObsIdSet.apply))

  def one(id: Observation.Id): ObsIdSet = ObsIdSet(NonEmptySet.one(id))

  def of(id: Observation.Id, ids: Observation.Id*): ObsIdSet = ObsIdSet(NonEmptySet.of(id, ids: _*))
}
