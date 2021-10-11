// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import lucuma.core.enum
import lucuma.core.model
import clue.annotation.GraphQLSchema
import io.circe.Encoder
import io.circe.Json
// gql: import lucuma.ui.reusability._

@GraphQLSchema
trait ITC {
  implicit val bdEncoder: Encoder[BigDecimal] =
    Encoder.encodeString.contramap[BigDecimal](_.toString)

  implicit val piEncoder: Encoder[eu.timepit.refined.types.numeric.PosInt] =
    Encoder.encodeString.contramap[eu.timepit.refined.types.numeric.PosInt](_.toString)

  implicit val spEncoder: Encoder[model.SpatialProfile] = new Encoder[model.SpatialProfile] {
    final def apply(a: model.SpatialProfile): Json = Json.obj(
      ("sourceType",
       Json.fromString(a match {
         case model.SpatialProfile.PointSource       => "POINT_SOURCE"
         case model.SpatialProfile.UniformSource     => "UNIFORm_SOURCE"
         case model.SpatialProfile.GaussianSource(_) => "GAUSSIAN_SOURCE"
       })
      ),
      ("fwhm",
       a match {
         case model.SpatialProfile.GaussianSource(f) =>
           Json.obj(("microarcseconds", Json.fromLong(f.toMicroarcseconds)))
         case _                                      => Json.Null
       }
      )
    )
  }

  object Scalars {
    // Basic types
    type BigDecimal = scala.BigDecimal
    type Long       = scala.Long
    // Time
    type Instant    = java.time.Instant
    // Refined
    type PosInt     = eu.timepit.refined.types.numeric.PosInt
  }
  object Enums   {
    type MagnitudeBand       = enum.MagnitudeBand
    type ImageQuality        = enum.ImageQuality
    type CloudExtinction     = enum.CloudExtinction
    type WaterVapor          = enum.WaterVapor
    type SkyBackground       = enum.SkyBackground
    type GmosNorthDisperser  = enum.GmosNorthDisperser
    type GmosNorthFilter     = enum.GmosNorthFilter
    type GmosNorthFpu        = enum.GmosNorthFpu
    type GmosCustomSlitWidth = enum.GmosCustomSlitWidth
  }

  object Types {
    type SpatialProfileModelInput = model.SpatialProfile
  }

}
