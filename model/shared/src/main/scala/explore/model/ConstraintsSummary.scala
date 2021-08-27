// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor

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

  implicit val eqConstraintSummary: Eq[ConstraintsSummary] =
    Eq.by(cs => (cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor))
}
