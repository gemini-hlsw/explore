// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import clue.macros.GraphQLSchema
import explore.model.SiderealTarget
import explore.model.enum._

object GraphQLSchemas {

  @GraphQLSchema(debug = false)
  object ObservationDB {
    object Scalars {
      type AsterismId    = String
      type BigDecimal    = scala.BigDecimal
      type DmsString     = String
      type EpochString   = String
      type HmsString     = String
      type Long          = scala.Long
      type ObservationId = String
      type ProgramId     = String
      type TargetId      = String
    }
  }

  @GraphQLSchema(debug = false)
  object ExploreDB {
    object Scalars {
      type Cloudcover       = CloudCover
      type Imagequality     = ImageQuality
      type Skybackground    = SkyBackground
      type Watervapor       = WaterVapor
      type Obsstatus        = ObsStatus
      type Targetobjecttype = TargetType
    }

    object Types {
      type TargetsInsertInput = SiderealTarget
      implicit final val jsonEncoderTargetsInsertInput: io.circe.Encoder[TargetsInsertInput] =
        explore.model.encoders.siderealTargetEncoder
    }
  }
}
