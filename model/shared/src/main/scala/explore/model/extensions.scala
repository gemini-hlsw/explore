// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.syntax.all.given
import lucuma.core.math.Epoch
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.model.SiderealTargetWithId
import lucuma.schemas.model.TargetWithId

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

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
    def toSiderealAt(vizTime: Instant): Option[Target.Sidereal] =
      toSidereal.map(_.at(vizTime))

    def toSidereal: Option[Target.Sidereal] =
      Target.sidereal.getOption(target)

  extension (targets: NonEmptyList[TargetWithId])
    def baseTracking: ObjectTracking =
      ObjectTracking.fromAsterism(targets.map(_.target))

    def toSidereal: List[SiderealTargetWithId] =
      targets.toList.map(_.toSidereal).flattenOption
