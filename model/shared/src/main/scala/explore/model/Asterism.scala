// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import monocle._
import lucuma.core.model.Target

final case class Asterism(targets: NonEmptyList[TargetWithId]) {
  def toSidereal: List[SiderealTargetWithId] =
    targets.traverse(_.toSidereal).foldMap(_.toList)

  def baseTarget: Option[TargetWithId] = targets.head.some

  def hasId(id: Target.Id): Boolean = targets.exists(_.id === id)
}

object Asterism {
  val isoTargets: Iso[NonEmptyList[TargetWithId], Asterism] =
    Iso[Asterism, NonEmptyList[TargetWithId]](_.targets)(Asterism.apply).reverse

  val targets: Lens[Asterism, NonEmptyList[TargetWithId]] = isoTargets.reverse.asLens

  val fromTargets: Lens[NonEmptyList[TargetWithId], Asterism] = isoTargets.asLens

  val targetsEach: Traversal[Asterism, TargetWithId] = targets.each

  val fromTargetsList: Iso[List[TargetWithId], Option[Asterism]] =
    Iso[List[TargetWithId], Option[Asterism]](fromTargets) {
      case Some(Asterism(targets)) => targets.toList
      case _                       => Nil
    }

  def fromTargets(targets: List[TargetWithId]): Option[Asterism] =
    NonEmptyList.fromList(targets).map(Asterism.apply)

  def of(targets: TargetWithId): Asterism =
    Asterism(NonEmptyList.of(targets))

  def targetOptional(targetId: Target.Id): Optional[Option[Asterism], Target] =
    Optional[Option[Asterism], Target](
      _.flatMap(_.targets.find(_.id === targetId).map(_.target))
    )(target =>
      _.map(
        Asterism.targetsEach.modify(twid =>
          if (twid.id === targetId) TargetWithId(targetId, target) else twid
        )
      )
    )

  implicit val eqAsterism: Eq[Asterism] = Eq.by(_.targets)
}
