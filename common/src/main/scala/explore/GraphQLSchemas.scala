// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import clue.data.syntax._
import clue.macros.GraphQLSchema
import explore.model.SiderealTarget
import explore.model.enum._
import lucuma.core.model.Magnitude
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import java.math.MathContext

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
      type ObservationId = Observation.Id
      type ProgramId     = String
      type TargetId      = Target.Id
    }

    object Enums {
      type CatalogName     = lucuma.core.enum.CatalogName
      type MagnitudeSystem = lucuma.core.enum.MagnitudeSystem
      type MagnitudeBand   = lucuma.core.enum.MagnitudeBand
    }

    object Implicits {
      import Types._

      implicit class MagnitudeOps(m: Magnitude) {
        def toInput: MagnitudeInput =
          MagnitudeInput(m.value.toDoubleValue,
                         m.band,
                         m.error.map(_.toRational.toBigDecimal(MathContext.UNLIMITED)).orIgnore,
                         m.system.assign
          )
      }
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

  @GraphQLSchema(debug = false)
  object UserPreferencesDB {
    object Scalars {
      type UserId        = User.Id
      type ResizableArea = String
    }

    object Types {
      type WidthInsertInput = ExploreResizableWidthInsertInput
      type WidthInsertInputL = Array[ExploreResizableWidthInsertInput]
      implicit final val jsonEncoderWidthInsertInput: io.circe.Encoder[WidthInsertInput] =
        ???
      implicit final val jsonEncoderWidthInsertInputL: io.circe.Encoder[Array[WidthInsertInput]] =
        ???
    }
  }
}
