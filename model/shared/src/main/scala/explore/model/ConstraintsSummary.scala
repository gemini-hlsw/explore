// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange

trait ConstraintsSummary {
  val imageQuality: ImageQuality
  val cloudExtinction: CloudExtinction
  val skyBackground: SkyBackground
  val waterVapor: WaterVapor

  def summaryString: String =
    s"${imageQuality.label} ${cloudExtinction.label} ${skyBackground.label} ${waterVapor.label}"
}

object ConstraintsSummary {
  val default: ConstraintsSummary = new ConstraintsSummary {
    // Defaults here should match server defaults for a smooth UI experience.
    val imageQuality    = ImageQuality.PointEight
    val cloudExtinction = CloudExtinction.PointThree
    val skyBackground   = SkyBackground.Bright
    val waterVapor      = WaterVapor.Wet
  }

  implicit class ConstraintSummaryOps(val cs: ConstraintsSummary) extends AnyVal {
    // Create a ConstraintSet which is not safe as the elevation range is not set
    def withDefaultElevationRange: ConstraintSet = ConstraintSet(cs.imageQuality,
                                                                 cs.cloudExtinction,
                                                                 cs.skyBackground,
                                                                 cs.waterVapor,
                                                                 ElevationRange.AirMass.Default
    )
  }

  implicit val eqConstraintSummary: Eq[ConstraintsSummary] =
    Eq.by(cs => (cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor))
}
