// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.refined.*
import monocle.Focus
import monocle.Lens

case class AsterismVisualOptions(
  id:         Option[Int],
  fovRA:      Angle,
  fovDec:     Angle,
  viewOffset: Offset,
  saturation: AsterismVisualOptions.ImageFilterRange,
  brightness: AsterismVisualOptions.ImageFilterRange
) derives Eq

object AsterismVisualOptions:
  type FilterRange      = Interval.Closed[0, 100]
  type ImageFilterRange = Int Refined FilterRange

  val id         = Focus[AsterismVisualOptions](_.id)
  val fovRA      = Focus[AsterismVisualOptions](_.fovRA)
  val fovDec     = Focus[AsterismVisualOptions](_.fovDec)
  val viewOffset = Focus[AsterismVisualOptions](_.viewOffset)
  val saturation = Focus[AsterismVisualOptions](_.saturation)
  val brightness = Focus[AsterismVisualOptions](_.brightness)

  val Default =
    AsterismVisualOptions(
      None,
      Constants.InitialFov,
      Constants.InitialFov,
      Offset.Zero,
      100.refined,
      100.refined
    )
