// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.syntax.all._
import lucuma.core.enum.GuideSpeed
import lucuma.core.math.Offset
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import fs2._
import lucuma.core.enum.Band
import lucuma.core.enum.ImageQuality
import lucuma.core.geom.Area
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Wavelength

object AGS {

  def runAnalysis(
    conditions: ConstraintSet,
    wavelength: Wavelength,
    gsOffset:   Offset,
    pos:        AgsPosition,
    params:     AgsParams,
    gsc:        GuideStarCandidate
  ): AgsAnalysis =
    if (!params.isReachable(gsOffset, pos))
      AgsAnalysis.NotReachable(pos, params.probe, gsc)
    else
      magnitudeAnalysis(
        conditions,
        params.probe,
        gsOffset,
        gsc,
        wavelength,
        // calculate vignetting
        params.vignettingArea(pos)(_).eval.area
      )

  /**
   * Analysis of the suitability of the magnitude of the given guide star regardless of its
   * reachability.
   */
  def magnitudeAnalysis(
    constraints:    ConstraintSet,
    guideProbe:     GuideProbe,
    gsOffset:       Offset,
    guideStar:      GuideStarCandidate,
    wavelength:     Wavelength,
    vignettingArea: Offset => Area
  ): AgsAnalysis = {
    import AgsGuideQuality._
    import AgsAnalysis._

    // Called when we know that a valid guide speed can be chosen for the given guide star.
    // Determine the quality and return an analysis indicating that the star is usable.
    def usable(guideSpeed: GuideSpeed): AgsAnalysis = {
      def worseOrEqual(iq: ImageQuality) = constraints.imageQuality >= iq

      val quality = guideSpeed match {
        case GuideSpeed.Fast   =>
          DeliversRequestedIq
        case GuideSpeed.Medium =>
          // if (worseOrEqual(ImageQuality.PERCENT_70)) DeliversRequestedIq
          if (worseOrEqual(ImageQuality.PointSix)) DeliversRequestedIq
          else PossibleIqDegradation
        case GuideSpeed.Slow   =>
          // if (worseOrEqual(ImageQuality.PERCENT_85)) DeliversRequestedIq
          if (worseOrEqual(ImageQuality.PointEight)) DeliversRequestedIq
          // else if (worseOrEqual(ImageQuality.PERCENT_70)) PossibleIqDegradation
          else if (worseOrEqual(ImageQuality.PointSix)) PossibleIqDegradation
          else IqDegradation
      }

      Usable(guideProbe, guideStar, guideSpeed.some, quality, vignettingArea(gsOffset))
    }

    // Do we have a g magnitude
    guideStar.gBrightness
      .map { g =>
        fastestGuideSpeed(constraints, wavelength, g)
          .map { speed =>
            usable(speed)
          }
          .getOrElse(NoGuideStarForProbe(guideProbe))
      }
      .getOrElse(NoMagnitudeForBand(guideProbe, guideStar))
  }

  /**
   * FS2 pipe to convert a stream of Sideral targets to analyzed guidestars
   */
  def agsAnalysisStream[F[_]](
    constraints:     ConstraintSet,
    wavelength:      Wavelength,
    baseCoordinates: Coordinates,
    position:        AgsPosition,
    params:          AgsParams
  ): Pipe[F, GuideStarCandidate, (GuideStarCandidate, AgsAnalysis)] =
    in =>
      in.map { gsc =>
        val offset = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset
        gsc -> runAnalysis(constraints, wavelength, offset, position, params, gsc)
      }

  /**
   * FS2 pipe to convert a stream of Sideral targets to analyzed guidestars
   */
  def agsAnalysis[F[_]](
    constraints:     ConstraintSet,
    wavelength:      Wavelength,
    baseCoordinates: Coordinates,
    position:        AgsPosition,
    params:          AgsParams,
    candidates:      List[GuideStarCandidate]
  ): List[(GuideStarCandidate, AgsAnalysis)] =
    candidates.map { gsc =>
      val offset = baseCoordinates.diff(gsc.tracking.baseCoordinates).offset
      gsc -> runAnalysis(constraints, wavelength, offset, position, params, gsc)
    }

  /**
   * Sort the guidde stars by analysis
   */
  def sortGuideStarCandidates(
    l: List[(GuideStarCandidate, Map[AgsPosition, AgsAnalysis])]
  ): Map[AgsPosition, Vector[(GuideStarCandidate, AgsAnalysis)]] =
    l.foldLeft(Map.empty[AgsPosition, Vector[(GuideStarCandidate, AgsAnalysis)]]) {
      case (map, (candidate, positions)) =>
        val results = positions.map { case (pos, analysis) =>
          pos -> map.getOrElse(pos, Vector.empty).appended((candidate -> analysis))
        }
        map ++ results
    }.map { case (k, v) =>
      implicit val ordering: Ordering[(GuideStarCandidate, AgsAnalysis)] = Ordering.by(_._2)
      k -> v.sorted
    }

  /**
   * Determines the fastest possible guide speed (if any) that may be used for guiding given a star
   * with the indicated magnitude.
   */
  def fastestGuideSpeed(
    constraints: ConstraintSet,
    wavelength:  Wavelength,
    magnitude:   BigDecimal
  ): Option[GuideSpeed] =
    GuideSpeed.all.find { speed => // assumes the values are sorted fast to slow
      gaiaBrightnessConstraints(constraints, speed, wavelength).contains(Band.Gaia, magnitude)
    }

}
