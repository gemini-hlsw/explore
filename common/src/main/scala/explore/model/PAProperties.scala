// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import crystal.react.View
import crystal.react.implicits.*
import explore.model.enums.AgsState
import explore.model.reusability.given
import japgolly.scalajs.react.Reusability
import lucuma.ags.AgsAnalysis
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.ui.reusability.given

case class PAProperties(
  oid:        Observation.Id,
  selectedGS: View[Option[AgsAnalysis]],
  agsState:   View[AgsState],
  constraint: View[Option[PosAngleConstraint]]
) {
  val selectedPA = selectedGS.get.posAngle
}

object PAProperties:
  given Reusability[PAProperties] =
    Reusability.by(x => (x.oid, x.selectedGS.get, x.agsState.get, x.constraint.get))
