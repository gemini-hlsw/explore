// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import java.time.Duration
import java.time.Instant

import cats.implicits._
import gem.enum.Site
import gem.enum.implicits._
import gsp.math.Coordinates
import gsp.math.skycalc.ImprovedSkyCalc
import gsp.math.skycalc.SkyCalcResults

object SkyCalc {
  // TODO Cache

  def forInterval(
    site:             Site,
    start:            Instant,
    end:              Instant,
    every:            Duration,
    coordsForInstant: Instant => Coordinates
  ): List[(Instant, SkyCalcResults)] = {
    val calc     = ImprovedSkyCalc(site.toPlace)
    val instants =
      List.unfold(start)(prev =>
        prev.plus(every).some.filter(_.isBefore(end)).map(i => (i, i))
      ) :+ end
    instants.map { i =>
      (i, calc.calculate(coordsForInstant(i), i, true))
    }
  }
}
