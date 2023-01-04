// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import crystal.react.View
import crystal.react.implicits.*
import explore.model.enums.AgsState
import explore.model.reusability.given
import japgolly.scalajs.react.Reusability
import lucuma.ags.AgsAnalysis
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.ui.reusability.given

extension (analysis: Option[AgsAnalysis])
  // Move to lucuma-catalog
  def posAngle: Option[Angle] = analysis match
    case Some(AgsAnalysis.Usable(_, _, _, _, v)) => Some(v.head._1.posAngle)
    case _                                       => None

case class PAProperties(
  oid:        Observation.Id,
  selectedGS: View[Option[AgsAnalysis]],
  agsState:   View[AgsState],
  constraint: View[Option[PosAngleConstraint]]
) {
  val selectedPA = selectedGS.get.posAngle
}

object PAProperties:
  // Move to lucuma-catalog
  given Reusability[AgsAnalysis] = Reusability {
    case (AgsAnalysis.Usable(a, b, c, d, e), AgsAnalysis.Usable(f, g, h, i, j)) =>
      (a === f) && (b === g) && (c === h) && (d === i) && (e === j)
    case _                                                                      => false
  }

  given Reusability[PAProperties] =
    Reusability.by(x => (x.oid, x.selectedGS.get, x.agsState.get, x.constraint.get))
