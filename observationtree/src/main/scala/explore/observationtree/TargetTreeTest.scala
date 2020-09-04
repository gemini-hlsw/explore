// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import java.time.Duration
import java.util.UUID

import cats.syntax.all._
import explore.model.Constraints
import explore.model.ExploreObservation
import explore.model.SiderealTarget
import explore.model.enum.ObsStatus
import explore.model.enum._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.ProperMotion
import lucuma.core.math.RightAscension

object TargetTreeTest {

  def target(name: String, raStr: String, decStr: String): SiderealTarget = {
    val ra     = RightAscension.fromStringHMS.getOption(raStr)
    val dec    = Declination.fromStringSignedDMS.getOption(decStr)
    val coords =
      ProperMotion((ra, dec).mapN(Coordinates.apply).getOrElse(Coordinates.Zero),
                   Epoch.J2000,
                   none,
                   none,
                   none
      )
    SiderealTarget(
      UUID.randomUUID,
      name,
      coords
    )
  }

  val ngc1055 = target("NGC 1055", "02:41:45.232999", "+00:26:35.450016")
  val ngc7752 = target("NGC 7752", "23:46:58.557000", "+29:27:32.169995")
  val ngc3705 = target("NGC 3705", "11:30:07.456000", "+09:16:35.870015")
  val ngc1068 = target("NGC 1068", "02:42:40.771000", "-00:00:47.840004")
  val ngc1087 = target("NGC 1087", "02:46:25.154457", "-00:29:55.449960")

  val targets = List(
    ngc1055,
    ngc7752,
    ngc3705,
    ngc1068,
    ngc1087
  )

  def constraints(name: String): Constraints =
    Constraints(UUID.randomUUID,
                name,
                CloudCover.Any,
                ImageQuality.Any,
                SkyBackground.Any,
                WaterVapor.Any
    )

  val constraints1 = constraints("<0.8\" <0.3 mag Gray")
  val constraints2 = constraints("<0.7\" <0.3 mag Bright")

  def observation(
    target:      SiderealTarget,
    conf:        String,
    constraints: Constraints,
    duration:    Duration
  ): ExploreObservation =
    ExploreObservation(
      UUID.randomUUID,
      target,
      ObsStatus.New,
      conf,
      constraints,
      duration
    )

  val observations: List[ExploreObservation] =
    List(
      observation(ngc1055, "GMOS-N R831 1x300\"", constraints1, Duration.ofHours(2)),
      observation(ngc7752, "GMOS-N R831 1x300\"", constraints1, Duration.ofMinutes(82)),
      observation(ngc1068, "GMOS-N R831 1x300\"", constraints1, Duration.ofMinutes(105)),
      observation(ngc1068, "GNIRS SXD 0.60\"", constraints2, Duration.ofMinutes(92))
    )
}
