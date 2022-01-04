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

  def fromList(obsIds: List[Observation.Id]): Option[ObsIdSet] =
    NonEmptySet.fromSet(SortedSet.from(obsIds)).map(ObsIdSet.apply)

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

  def of(id: Observation.Id, ids: Observation.Id*): ObsIdSet = ObsIdSet(NonEmptySet.of(id, ids: _*))
}
