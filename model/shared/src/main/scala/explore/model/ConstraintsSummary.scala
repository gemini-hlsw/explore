// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.model.ConstraintSet

trait ConstraintsSummary {
  val id: ConstraintSet.Id
  val name: NonEmptyString
  val imageQuality: ImageQuality
  val cloudExtinction: CloudExtinction
  val skyBackground: SkyBackground
  val waterVapor: WaterVapor

  def summaryString: String =
    s"${imageQuality.label} ${cloudExtinction.label} ${skyBackground.label} ${waterVapor.label}"
}

object ConstraintsSummary {
  implicit val eqConstraintSummary: Eq[ConstraintsSummary] =
    Eq.by(cs =>
      (cs.id, cs.name.value, cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor)
    )
}
