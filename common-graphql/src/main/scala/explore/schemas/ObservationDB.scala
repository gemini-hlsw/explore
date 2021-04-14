// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.annotation.GraphQLSchema
import lucuma.core.model.Asterism
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Target
// gql: import io.circe.refined._

@GraphQLSchema
trait ObservationDB {
  object Scalars {
    type AsterismId      = Asterism.Id
    type BigDecimal      = scala.BigDecimal
    type ConstraintSetId = ConstraintSet.Id
    type DmsString       = String
    type EpochString     = String
    type HmsString       = String
    type Long            = scala.Long
    type ObservationId   = Observation.Id
    type ProgramId       = String
    type TargetId        = Target.Id
    type NonEmptyString  = eu.timepit.refined.types.string.NonEmptyString
  }

  object Enums {
    type CatalogName     = lucuma.core.enum.CatalogName
    type MagnitudeSystem = lucuma.core.enum.MagnitudeSystem
    type MagnitudeBand   = lucuma.core.enum.MagnitudeBand
    type ImageQuality    = lucuma.core.enum.ImageQuality
    type CloudExtinction = lucuma.core.enum.CloudExtinction
    type SkyBackground   = lucuma.core.enum.SkyBackground
    type WaterVapor      = lucuma.core.enum.WaterVapor
  }
}
