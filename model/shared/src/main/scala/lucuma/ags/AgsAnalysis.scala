// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order
import cats.syntax.all._
import lucuma.core.enum.Band
import lucuma.core.enum.GuideSpeed
import lucuma.core.geom.Area

sealed trait AgsAnalysis {
  def quality: AgsGuideQuality = AgsGuideQuality.Unusable
  def isUsable: Boolean        = quality =!= AgsGuideQuality.Unusable
  def message(withProbe: Boolean): String
}

object AgsAnalysis {

  case object NotAnalized extends AgsAnalysis {
    override def message(withProbe: Boolean): String = "Not analyzed yet"
  }

  final case class NoGuideStarForProbe(guideProbe: GuideProbe) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"$guideProbe " else ""
      s"No ${p}guide star selected."
    }
  }

  final case class MagnitudeTooFaint(
    guideProbe:     GuideProbe,
    target:         GuideStarCandidate,
    showGuideSpeed: Boolean
  ) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p  = if (withProbe) s"use $guideProbe" else "guide"
      val gs = if (showGuideSpeed) ", even using the slowest guide speed" else ""
      s"Cannot $p with the star in these conditions$gs."
    }
  }

  final case class MagnitudeTooBright(guideProbe: GuideProbe, target: GuideStarCandidate)
      extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"$guideProbe g" else "G"
      s"${p}uide star is too bright to guide."
    }
  }

  final case class NotReachable(
    position:   AgsPosition,
    guideProbe: GuideProbe,
    target:     GuideStarCandidate
  ) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"with ${guideProbe} " else ""
      s"The star is not reachable ${p}at all positions."
    }
  }

  object NotReachable {
    implicit val order: Order[NotReachable] =
      Order.allEqual
  }

  final case class NoMagnitudeForBand(guideProbe: GuideProbe, target: GuideStarCandidate)
      extends AgsAnalysis {
    private val probeBands: List[Band]               = Nil // guideProbe.getBands
    override def message(withProbe: Boolean): String = {
      val p = if (withProbe) s"${guideProbe} g" else "G"
      if (probeBands.length == 1) {
        s"${p}uide star ${probeBands.head}-band magnitude is missing. Cannot determine guiding performance."
      } else {
        s"${p}uide star ${probeBands.map(_.shortName).mkString(", ")}-band magnitudes are missing. Cannot determine guiding performance."
      }
    }
    override val quality: AgsGuideQuality            = AgsGuideQuality.PossiblyUnusable
  }

  object NoMagnitudeForBand {
    implicit val order: Order[NoMagnitudeForBand] =
      Order.allEqual
  }

  final case class Usable(
    guideProbe:           GuideProbe,
    target:               GuideStarCandidate,
    guideSpeed:           Option[GuideSpeed],
    override val quality: AgsGuideQuality,
    vignettingArea:       Area
  ) extends AgsAnalysis {
    override def message(withProbe: Boolean): String = {
      val qualityMessage = quality match {
        case AgsGuideQuality.DeliversRequestedIq => ""
        case _                                   => s"${quality.message} "
      }
      val p              = if (withProbe) s"${guideProbe} " else ""
      val gs             = guideSpeed.fold("Usable")(gs => s"Guide Speed: ${gs.toString()}")
      s"$qualityMessage$p$gs. vignetting: ${vignettingArea.toMicroarcsecondsSquared} µas^2"
      s"$p $quality $gs. vignetting: ${vignettingArea.toMicroarcsecondsSquared} µas^2"
    }
  }

  object Usable {
    implicit val order: Order[Usable] =
      Order.by(u => (u.guideSpeed, u.quality, u.vignettingArea))
  }

  implicit val order: Order[AgsAnalysis] =
    Order.from {
      case (a: Usable, b: Usable) => Usable.order.compare(a, b)
      case (_: Usable, _)         => Int.MinValue
      case (_, _: Usable)         => Int.MaxValue
      case _                      => Int.MinValue
    }

  implicit val ordering: Ordering[AgsAnalysis] = order.toOrdering

}
