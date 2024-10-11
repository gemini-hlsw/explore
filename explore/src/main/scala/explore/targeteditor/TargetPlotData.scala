// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.model.CoordinatesAtVizTime

case class TargetPlotData(
  name:   NonEmptyString,
  coords: CoordinatesAtVizTime,
  style:  TargetPlotData.Style
) derives Eq

object TargetPlotData:
  enum Style derives Eq:
    case Solid, Dashed

  given Reusability[TargetPlotData] = Reusability.byEq
