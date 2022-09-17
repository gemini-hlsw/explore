// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.data.Zipper
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.refined.*
import monocle.*

import java.time.Instant

/**
 * Generic representation to track an object. It is generalization of SiderealTracking but allows
 * tracking "virtual" objects like the center of an asterism
 */
sealed trait ObjectTracking derives Eq {
  def at(i: Instant): Option[Coordinates]
  def baseCoordinates: Coordinates
}

object ObjectTracking {
  case class ConstantTracking(coord: Coordinates) extends ObjectTracking derives Eq {
    def at(i: Instant): Option[Coordinates] = coord.some
    def baseCoordinates: Coordinates        = coord
  }

  case class SiderealObjectTracking(tracking: SiderealTracking) extends ObjectTracking derives Eq {
    def at(i: Instant): Option[Coordinates] = tracking.at(i)

    def baseCoordinates: Coordinates = tracking.baseCoordinates
  }

  def const(coord: Coordinates): ObjectTracking = ConstantTracking(coord)

  def fromTarget(target: Target): ObjectTracking = target match {
    case t: Target.Sidereal => SiderealObjectTracking(t.tracking)
    case _                  => sys.error("Only sidereal targets supported")
  }

}

extension (a: NonEmptyList[TargetWithId])
  def centerOf: Coordinates =
    val coords = a.map(_.toSidereal).collect { case Some(x) =>
      x.target.tracking.baseCoordinates
    }
    Coordinates.centerOf(coords)

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

  def head = targets.head

  def toZipper(id: Target.Id): Option[Zipper[TargetWithId]] =
    Zipper.fromNel(targets).findFocus(_.id === id)

  def baseTracking: ObjectTracking =
    if (targets.length > 1)
      ObjectTracking.const(targets.centerOf)
    else ObjectTracking.fromTarget(targets.head.target)

  def hasId(id: Target.Id): Boolean = targets.exists(_.id === id)
}

object Asterism {
  val isoTargets: Iso[NonEmptyList[TargetWithId], Asterism] =
    Iso[Asterism, NonEmptyList[TargetWithId]](_.targets)(Asterism.apply).reverse

  def oneTarget(id: Target.Id): Iso[Asterism, Target] =
    Iso[Asterism, Target](_.targets.head.target)(t => Asterism.one(TargetWithId(id, t)))

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
