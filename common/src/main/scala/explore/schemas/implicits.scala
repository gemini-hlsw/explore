// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.data.syntax._
import lucuma.core.model.Magnitude

import java.math.MathContext

import ObservationDB.Types.MagnitudeCreateInput
import UserPreferencesDB.Types.ExploreResizableWidthInsertInput

object implicits {
  implicit class MagnitudeOps(m: Magnitude) {
    def toCreateInput: MagnitudeCreateInput =
      MagnitudeCreateInput(m.band,
                           m.value.toDoubleValue,
                           m.error.map(_.toRational.toBigDecimal(MathContext.UNLIMITED)).orIgnore,
                           m.system.assign
      )
  }

  implicit def widthUpsertInput(w: WidthUpsertInput): ExploreResizableWidthInsertInput =
    ExploreResizableWidthInsertInput(
      w.section.value.assign,
      w.user.toString.assign,
      w.width.assign
    )
}
