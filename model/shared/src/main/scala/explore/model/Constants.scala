// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.Angle

import java.time.ZoneOffset

trait Constants:
  val UTC                      = ZoneOffset.UTC
  val TwoPanelCutoff           = 576.0
  val InitialTreeWidth         = 300.0
  val MinLeftPanelWidth        = 270.0
  val GridRowHeight            = 36
  val GridRowPadding           = 5
  val GridColCount             = 12
  // 4 arcmin
  val PreviewFov: Angle        = Angle.fromMicroarcseconds(240000000L)
  // 10 arcmin
  val InitialFov: Angle        = Angle.fromMicroarcseconds(600000000L)
  val SimbadResultLimit        = 50
  val MaxConcurrentItcRequests = 4

  val Calculating        = "Calculating..."
  val LoadingStars       = "Loading candidate stars..."
  val NoGuideStarMessage = "No guidestar available"
  val NoDuration         = "No duration available"
  val NoExposureTimeMode = "No exposure time mode defined"
  val MissingMode        = "Observation is missing observing mode" // Matches odb error message
  val MissingCandidates  = "No catalog stars available"
  val NoObservations     = "No observations available"
  val NoTargets          = "No targets available"
  val NoTargetSelected   = "No target selected"
  val BadTimingWindow    = "Review the dates on this timing window."
  val MissingInfoMsg     = "Not enough information to call ITC"
  val P1TemplatesUrl     = "https://www.gemini.edu/observing/phase-i/pit/pit-description#PDF"

  val SignalToNoiseAtLabel = "λ for S/N"

object Constants extends Constants
