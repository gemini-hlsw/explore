// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.syntax.all.given
import lucuma.core.math.Arc
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.model.SiderealTargetWithId
import lucuma.schemas.model.TargetWithId

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import scala.annotation.targetName

object extensions:
  // TODO Move this to lucuma-schemas (and remove this logic from TargetWithId)
  extension (target: Target.Sidereal)
    def at(i: Instant): Target.Sidereal = {
      val ldt            = LocalDateTime.ofInstant(i, ZoneOffset.UTC)
      val epoch          = Epoch.Julian.fromLocalDateTime(ldt).getOrElse(target.tracking.epoch)
      val trackingUpdate = (tracking: SiderealTracking) =>
        tracking.at(i).fold(tracking) { c =>
          val update = SiderealTracking.baseCoordinates.replace(c) >>> SiderealTracking.epoch
            .replace(epoch)
          update(tracking)
        }

      Target.Sidereal.tracking.modify(trackingUpdate)(target)
    }

  extension (target: Target)
    // If the target is sidereal, update it to the given instant.
    // Someday we may need to handle nonsidereal targets...
    def at(i: Instant): Target = target match
      case st @ Target.Sidereal(_, _, _, _) => st.at(i)
      case Target.Nonsidereal(_, _, _)      => target
      case Target.Opportunity(_, _, _)      => target

    // When we have nonsidereals...
    def coordsOrRegion: Option[Either[Coordinates, Region]] = target match
      case Target.Sidereal(_, tracking, _, _) => tracking.baseCoordinates.asLeft.some
      case Target.Nonsidereal(_, _, _)        => none
      case Target.Opportunity(_, region, _)   => region.asRight.some

  extension (targets: NonEmptyList[TargetWithId])
    def baseTracking: Option[ObjectTracking] =
      ObjectTracking.fromAsterism(targets.map(_.target))

    def toSidereal: List[SiderealTargetWithId] =
      targets.toList.map(_.toSidereal).flattenOption

  extension [A](arc: Arc[A])
    def format(f: A => String): String = arc match
      case Arc.Empty()             => "Empty"
      case Arc.Full()              => "Full"
      case Arc.Partial(start, end) => s"${f(start)} - ${f(end)}"

  extension (coordsOrRegion: Option[Either[Coordinates, Region]])
    def ra: Option[Either[RightAscension, Arc[RightAscension]]] =
      coordsOrRegion.map(_.bimap(_.ra, _.raArc))
    def dec: Option[Either[Declination, Arc[Declination]]]      =
      coordsOrRegion.map(_.bimap(_.dec, _.decArc))

  extension (raOrArc: Option[Either[RightAscension, Arc[RightAscension]]])
    @targetName("formatRA")
    def format(f: RightAscension => String): String = raOrArc match
      case None                                  => ""
      case Some(Left(ra))                        => f(ra)
      case Some(Right(arc: Arc[RightAscension])) => arc.format(f)

  extension (decOrArc: Option[Either[Declination, Arc[Declination]]])
    @targetName("formatDec")
    def format(f: Declination => String): String = decOrArc match
      case None                               => ""
      case Some(Left(dec))                    => f(dec)
      case Some(Right(arc: Arc[Declination])) => arc.format(f)
