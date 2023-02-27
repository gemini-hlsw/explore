// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.*
import explore.model.AladinFullScreen
import explore.model.enums.Visible
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.refined.*
import monocle.Focus
import monocle.Lens

case class TargetVisualOptions(
  fovRA:              Angle,
  fovDec:             Angle,
  viewOffset:         Offset,
  agsCandidates:      Visible,
  agsOverlay:         Visible,
  fullScreen:         AladinFullScreen,
  saturation:         TargetVisualOptions.ImageFilterRange,
  brightness:         TargetVisualOptions.ImageFilterRange,
  scienceOffsets:     Visible,
  acquisitionOffsets: Visible
) derives Eq

object TargetVisualOptions:
  type FilterRange      = Interval.Closed[0, 100]
  type ImageFilterRange = Int Refined FilterRange
  val fovRA              = Focus[TargetVisualOptions](_.fovRA)
  val fovDec             = Focus[TargetVisualOptions](_.fovDec)
  val viewOffset         = Focus[TargetVisualOptions](_.viewOffset)
  val agsCandidates      = Focus[TargetVisualOptions](_.agsCandidates)
  val agsOverlay         = Focus[TargetVisualOptions](_.agsOverlay)
  val fullScreen         = Focus[TargetVisualOptions](_.fullScreen)
  val saturation         = Focus[TargetVisualOptions](_.saturation)
  val brightness         = Focus[TargetVisualOptions](_.brightness)
  val scienceOffsets     = Focus[TargetVisualOptions](_.scienceOffsets)
  val acquisitionOffsets = Focus[TargetVisualOptions](_.acquisitionOffsets)

  val Default =
    TargetVisualOptions(
      Constants.InitialFov,
      Constants.InitialFov,
      Offset.Zero,
      Visible.Inline,
      Visible.Inline,
      AladinFullScreen.Normal,
      100.refined,
      100.refined,
      Visible.Inline,
      Visible.Inline
    )
