// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.model.ConstraintSet
import monocle.macros.Lenses

@Lenses
final case class ConstraintsSummary(
  name:            NonEmptyString,
  id:              ConstraintSet.Id,
  imageQuality:    ImageQuality,
  cloudExtinction: CloudExtinction,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor
) {
  def summaryString: String =
    s"${imageQuality.label} ${cloudExtinction.label} ${skyBackground.label} ${waterVapor.label}"
}

object ConstraintsSummary {

  implicit val constraintSummaryDecoder: Decoder[ConstraintsSummary] = deriveDecoder
  implicit val constraintSummaryEncoder: Encoder[ConstraintsSummary] = deriveEncoder

  implicit val eqConstraintSummary: Eq[ConstraintsSummary] = Eq.by(cs =>
    (cs.name, cs.id, cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor)
  )
}
