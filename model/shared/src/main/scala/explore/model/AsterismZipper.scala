// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.data.Zipper
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.*
import monocle.*
import explore.model.extensions.*

import java.time.Instant

/**
 * Contains a list of targets focused on the selected one on the UI
 */
// Can this be an opaque type?
// Maybe this can be a wrapper for a View[Asterism] and a selected id that must be in the view
case class AsterismZipper(private val targets: Zipper[TargetWithId]) derives Eq {
  def toSiderealAt(vizTime: Instant): List[SiderealTargetWithId] =
    targets.traverse(_.toSidereal.map(_.at(vizTime))).foldMap(_.toList)

  def toSidereal: List[SiderealTargetWithId] =
    targets.toNel.toSidereal

  def toSiderealTracking: List[SiderealTracking] =
    targets.traverse(_.toSidereal.map(_.target.tracking)).foldMap(_.toList)

  def asList: List[TargetWithId] = targets.toList

  def add(t: TargetWithId): AsterismZipper =
    AsterismZipper.isoTargets.reverse.modify(_ :+ t)(this)

  def ids: NonEmptyList[Target.Id] = targets.toNel.map(_.id)

  def remove(id: Target.Id): Option[AsterismZipper] =
    if (hasId(id)) {
      val filtered = targets.toNel.filter(_.id =!= id)
      AsterismZipper.fromTargets(filtered)
    } else this.some

  def focus = targets.focus

  def focusOn(tid: Target.Id): AsterismZipper =
    targets.findFocus(_.id === tid).map(AsterismZipper.apply).getOrElse(this)

  def baseTrackingAt(vizTime: Instant): Option[ObjectTracking] =
    targets.toNel.baseTrackingAt(vizTime)

  def baseTracking: ObjectTracking = targets.toNel.baseTracking

  def hasId(id: Target.Id): Boolean = targets.exists(_.id === id)
}

object AsterismZipper {
  val isoTargets: Iso[NonEmptyList[TargetWithId], AsterismZipper] =
    Iso[AsterismZipper, NonEmptyList[TargetWithId]](_.targets.toNel)(s =>
      AsterismZipper(Zipper.fromNel(s))
    ).reverse

  // TODO I think this is unlawful. Not all AsterismZippers have one element.
  def oneTarget(id: Target.Id): Iso[AsterismZipper, Target] =
    Iso[AsterismZipper, Target](_.targets.focus.target)(t =>
      AsterismZipper.one(TargetWithId(id, t))
    )

  val targetsEach: Traversal[AsterismZipper, TargetWithId] = isoTargets.reverse.each

  val targets: Lens[AsterismZipper, Zipper[TargetWithId]] = Focus[AsterismZipper](_.targets)

  val focus: Lens[AsterismZipper, TargetWithId] = targets.andThen(Zipper.focus)

  def fromTargets(targets: List[TargetWithId]): Option[AsterismZipper] =
    NonEmptyList.fromList(targets).map(s => AsterismZipper(Zipper.fromNel(s)))

  val siderealTargetsEach: Traversal[AsterismZipper, SiderealTargetWithId] =
    targetsEach.andThen(TargetWithId.sidereal)

  val fromTargetsList: Iso[List[TargetWithId], Option[AsterismZipper]] =
    Iso[List[TargetWithId], Option[AsterismZipper]](fromTargets) {
      case Some(AsterismZipper(targets)) => targets.toList
      case _                             => Nil
    }

  def fromTargetsListOn(id: Option[Target.Id]): Iso[List[TargetWithId], Option[AsterismZipper]] =
    Iso[List[TargetWithId], Option[AsterismZipper]]((tl: List[TargetWithId]) =>
      fromTargets(tl).flatMap(a => id.map(a.focusOn).orElse(a.some))
    ) {
      case Some(AsterismZipper(targets)) => targets.toList
      case _                             => Nil
    }

  def one(targets: TargetWithId): AsterismZipper =
    AsterismZipper(Zipper.of(targets))

  def targetOptional(targetId: Target.Id): Optional[Option[AsterismZipper], TargetWithId] =
    Optional[Option[AsterismZipper], TargetWithId](
      _.flatMap(_.targets.find(_.id === targetId))
    )(target =>
      _.map(
        AsterismZipper.targetsEach.modify(twid => if (twid.id === targetId) target else twid)
      )
    )

}
