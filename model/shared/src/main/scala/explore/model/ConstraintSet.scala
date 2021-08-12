// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.cats._
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import monocle.Focus

final case class ConstraintSet(
  imageQuality:    ImageQuality,
  cloudExtinction: CloudExtinction,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRange
) {
  def displayName: String = {
    val wv = if (waterVapor === WaterVapor.Wet) "" else s" ${waterVapor.label}"
    val er = elevationRange match {
      case AirMassRange(min, max)
          if min === AirMassRange.DefaultMin && max === AirMassRange.DefaultMax =>
        ""
      case AirMassRange(min, max) if min === AirMassRange.DefaultMin     => f" AM<${max.value}%.1f"
      case AirMassRange(min, max) if max === AirMassRange.DefaultMax     => f" ${min.value}%.1f<AM"
      case AirMassRange(min, max)                                        => f" ${min.value}%.1fAM<${max.value}%.1f"
      case HourAngleRange(min, max) if min === HourAngleRange.DefaultMin => f" HA<${max.value}%.1f"
      case HourAngleRange(min, max) if max === HourAngleRange.DefaultMin => f" ${min.value}%.1f<HA"
      case HourAngleRange(min, max)                                      => f" ${min.value}%.1f<HA<${max.value}%.1f"
    }

    s"${imageQuality.label} ${cloudExtinction.label} ${skyBackground.label}$wv$er"
  }
}

object ConstraintSet {
  val imageQuality    = Focus[ConstraintSet](_.imageQuality)
  val cloudExtinction = Focus[ConstraintSet](_.cloudExtinction)
  val skyBackground   = Focus[ConstraintSet](_.skyBackground)
  val waterVapor      = Focus[ConstraintSet](_.waterVapor)
  val elevationRange  = Focus[ConstraintSet](_.elevationRange)

  implicit val decoderConstraintsSet: Decoder[ConstraintSet] = deriveDecoder

  implicit val eqConstraintsSet: Eq[ConstraintSet] = Eq.by(cs =>
    (cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor, cs.elevationRange)
  )
}
