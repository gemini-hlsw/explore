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

extension (a: NonEmptyList[TargetWithId])
  def centerOf: Coordinates =
    val coords = a.map(_.toSidereal).collect { case Some(x) =>
      x.target.tracking.baseCoordinates
    }
    Coordinates.centerOf(coords)

/**
 * Contains a list of targets focused on the selected one on the UI
 */
case class Asterism(private val targets: Zipper[TargetWithId]) derives Eq {
  def toSiderealAt(vizTime: Instant): List[SiderealTargetWithId] =
    targets.traverse(_.toSidereal.map(_.at(vizTime))).foldMap(_.toList)

  def asList: List[TargetWithId] = targets.toList

  def add(t: TargetWithId): Asterism =
    Asterism.isoTargets.reverse.modify(_ :+ t)(this)

  def ids: NonEmptyList[Target.Id] = targets.toNel.map(_.id)

  def remove(id: Target.Id): Option[Asterism] =
    if (hasId(id)) {
      val filtered = targets.toNel.filter(_.id =!= id)
      Asterism.fromTargets(filtered)
    } else this.some

  def focus = targets.focus

  def focusOn(tid: Target.Id): Asterism =
    targets.findFocus(_.id === tid).map(Asterism.apply).getOrElse(this)

  def baseTracking: ObjectTracking =
    if (targets.length > 1)
      ObjectTracking.const(targets.toNel.centerOf)
    else ObjectTracking.fromTarget(targets.focus.target)

  def hasId(id: Target.Id): Boolean = targets.exists(_.id === id)
}

object Asterism {
  val isoTargets: Iso[NonEmptyList[TargetWithId], Asterism] =
    Iso[Asterism, NonEmptyList[TargetWithId]](_.targets.toNel)(s =>
      Asterism(Zipper.fromNel(s))
    ).reverse

  def oneTarget(id: Target.Id): Iso[Asterism, Target] =
    Iso[Asterism, Target](_.targets.focus.target)(t => Asterism.one(TargetWithId(id, t)))

  val targetsEach: Traversal[Asterism, TargetWithId] = isoTargets.reverse.each

  val targets: Lens[Asterism, Zipper[TargetWithId]] = Focus[Asterism](_.targets)

  val focus: Lens[Asterism, TargetWithId] = targets.andThen(Zipper.focus)

  val siderealTargetsEach: Traversal[Asterism, SiderealTargetWithId] =
    targetsEach.andThen(TargetWithId.sidereal)

  val fromTargetsList: Iso[List[TargetWithId], Option[Asterism]] =
    Iso[List[TargetWithId], Option[Asterism]](fromTargets) {
      case Some(Asterism(targets)) => targets.toList
      case _                       => Nil
    }

  def fromTargets(targets: List[TargetWithId]): Option[Asterism] =
    NonEmptyList.fromList(targets).map(s => Asterism(Zipper.fromNel(s)))

  def one(targets: TargetWithId): Asterism =
    Asterism(Zipper.of(targets))

  def targetOptional(targetId: Target.Id): Optional[Option[Asterism], TargetWithId] =
    Optional[Option[Asterism], TargetWithId](
      _.flatMap(_.targets.find(_.id === targetId))
    )(target =>
      _.map(
        Asterism.targetsEach.modify(twid => if (twid.id === targetId) target else twid)
      )
    )

}
