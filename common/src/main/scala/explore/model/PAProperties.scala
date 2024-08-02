// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import crystal.react.*
import explore.model.enums.AgsState
import japgolly.scalajs.react.ReactCats.*
import lucuma.ags.AgsAnalysis
import lucuma.core.model.PosAngleConstraint

case class PAProperties(
  oid:        Observation.Id,
  selectedGS: View[Option[AgsAnalysis]],
  agsState:   View[AgsState],
  constraint: View[PosAngleConstraint]
) {
  val selectedPA = selectedGS.get.posAngle
}

object PAProperties:
  given Eq[PAProperties] =
    Eq.by(x => (x.oid, x.selectedGS.get, x.agsState.get, x.constraint.get))
