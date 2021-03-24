package explore.schemas

import clue.data.syntax._
import lucuma.core.model.Magnitude
import ObservationDB.Types.MagnitudeInput
import java.math.MathContext
import UserPreferencesDB.Types.WidthUpsertInput
import UserPreferencesDB.Types.ExploreResizableWidthInsertInput

object implicits {
  implicit class MagnitudeOps(m: Magnitude) {
    def toInput: MagnitudeInput =
      MagnitudeInput(m.value.toDoubleValue,
                     m.band,
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
