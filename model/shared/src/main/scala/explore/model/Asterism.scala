// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.model.Target
import monocle.*
import lucuma.core.math.Coordinates
import lucuma.core.data.Zipper

import java.time.Instant

case class Asterism(private val targets: NonEmptyList[TargetWithId]) derives Eq {
  def toSiderealAt(vizTime: Instant): List[SiderealTargetWithId] =
    targets.traverse(_.toSidereal.map(_.at(vizTime))).foldMap(_.toList)

  def asList: List[TargetWithId] = targets.toList

  def add(t: TargetWithId): Asterism =
    Asterism.isoTargets.reverse.modify(_ :+ t)(this)

  def ids: NonEmptyList[Target.Id] = targets.map(_.id)

  def remove(id: Target.Id): Option[Asterism] =
    if (hasId(id)) {
      val filtered = targets.filter(_.id =!= id)
      Asterism.fromTargets(filtered)
    } else this.some

  def baseCoordinatesAt(i: Instant): Option[Coordinates] = none

  def baseCoordinates: Coordinates = ???

  def toZipper(id: Target.Id): Option[Zipper[TargetWithId]] =
    Zipper.fromNel(targets).findFocus(_.id === id)

  // This should be calculatedd from the other targets or manually overriden
  def baseTarget: TargetWithId = targets.head

  def baseCoordinatesAt(i: Instant): Option[Coordinates] = none

  def baseCoordinates: Coordinates = ???

  def hasId(id: Target.Id): Boolean = targets.exists(_.id === id)
}

object Asterism {
  val isoTargets: Iso[NonEmptyList[TargetWithId], Asterism] =
    Iso[Asterism, NonEmptyList[TargetWithId]](_.targets)(Asterism.apply).reverse

  val targetsEach: Traversal[Asterism, TargetWithId] = isoTargets.reverse.each

  val siderealTargetsEach: Traversal[Asterism, SiderealTargetWithId] =
    targetsEach.andThen(TargetWithId.sidereal)

  val fromTargetsList: Iso[List[TargetWithId], Option[Asterism]] =
    Iso[List[TargetWithId], Option[Asterism]](fromTargets) {
      case Some(Asterism(targets)) => targets.toList
      case _                       => Nil
    }

  def toZipperLens(id: Target.Id): Lens[Asterism, Option[Zipper[TargetWithId]]] =
    Lens[Asterism, Option[Zipper[TargetWithId]]](_.toZipper(id))(z =>
      a => z.map(z => Asterism(z.toNel)).getOrElse(a)
    )

  def fromTargets(targets: List[TargetWithId]): Option[Asterism] =
    NonEmptyList.fromList(targets).map(Asterism.apply)

  def one(targets: TargetWithId): Asterism =
    Asterism(NonEmptyList.one(targets))

  def targetOptional(targetId: Target.Id): Optional[Option[Asterism], TargetWithId] =
    Optional[Option[Asterism], TargetWithId](
      _.flatMap(_.targets.find(_.id === targetId))
    )(target =>
      _.map(
        Asterism.targetsEach.modify(twid => if (twid.id === targetId) target else twid)
      )
    )

}
