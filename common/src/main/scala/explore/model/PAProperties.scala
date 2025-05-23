// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import crystal.react.*
import explore.model.enums.AgsState
import lucuma.core.model.PosAngleConstraint

case class PAProperties(
  oid:                Observation.Id,
  guideStarSelection: View[GuideStarSelection],
  agsState:           View[AgsState],
  constraint:         PosAngleConstraint
):
  val selectedPA = guideStarSelection.get.selectedAngle

object PAProperties:
  given Eq[PAProperties] =
    Eq.by(x => (x.oid, x.guideStarSelection.get, x.agsState.get, x.constraint))
