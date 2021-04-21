// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._
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
  waterVapor:      WaterVapor,
  obsCount:        NonNegInt
) {
  def summaryString: String =
    s"${imageQuality.label} ${cloudExtinction.label} ${skyBackground.label} ${waterVapor.label}"
}

object ConstraintsSummary {
  def default(name: NonEmptyString, id: ConstraintSet.Id): ConstraintsSummary =
    ConstraintsSummary(
      id = id,
      name = name,
      imageQuality = ImageQuality.PointEight,
      cloudExtinction = CloudExtinction.PointThree,
      skyBackground = SkyBackground.Gray,
      waterVapor = WaterVapor.Wet,
      obsCount = 0
    )

  implicit val constraintSummaryDecoder: Decoder[ConstraintsSummary] =
    new Decoder[ConstraintsSummary] {
      final def apply(c: HCursor): Decoder.Result[ConstraintsSummary] =
        for {
          name <- c.downField("name").as[NonEmptyString]
          id   <- c.downField("id").as[ConstraintSet.Id]
          iq   <- c.downField("imageQuality").as[ImageQuality]
          ce   <- c.downField("cloudExtinction").as[CloudExtinction]
          sb   <- c.downField("skyBackground").as[SkyBackground]
          wv   <- c.downField("waterVapor").as[WaterVapor]
          oc   <- c.downField("obsCount").downField("totalCount").as[NonNegInt]
        } yield new ConstraintsSummary(name, id, iq, ce, sb, wv, oc)
    }

  implicit val constraintSummaryEncoder: Encoder[ConstraintsSummary] =
    new Encoder[ConstraintsSummary] {
      final def apply(a: ConstraintsSummary): Json = Json.obj(
        ("name", a.name.asJson),
        ("id", a.id.asJson),
        ("imageQuality", a.imageQuality.asJson),
        ("cloudExtinction", a.cloudExtinction.asJson),
        ("skyBackground", a.skyBackground.asJson),
        ("waterVapor", a.waterVapor.asJson),
        ("obsCount", Json.obj("totalCount" -> a.obsCount.asJson))
      )
    }

  implicit val eqConstraintSummary: Eq[ConstraintsSummary] = Eq.by(cs =>
    (cs.name,
     cs.id,
     cs.imageQuality,
     cs.cloudExtinction,
     cs.skyBackground,
     cs.waterVapor,
     cs.obsCount
    )
  )
}
