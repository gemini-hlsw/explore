// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import crystal.react.*
import explore.model.enums.AgsState
import japgolly.scalajs.react.ReactCats.*
import lucuma.core.model.PosAngleConstraint

case class PAProperties(
  oid:        Observation.Id,
  agsState:   View[AgsState],
  constraint: View[PosAngleConstraint]
)

object PAProperties:
  given Eq[PAProperties] =
    Eq.by(x => (x.oid, x.agsState.get, x.constraint.get))
