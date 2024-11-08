// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.plots

import cats.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.math.skycalc.SkyCalcResults

import java.time.Duration
import java.time.Instant

object SkyCalc:
  // TODO Cache?

  def forInterval(
    site:             Site,
    start:            Instant,
    end:              Instant,
    every:            Duration,
    coordsForInstant: Instant => Coordinates
  ): List[(Instant, SkyCalcResults)] =
    val calc = ImprovedSkyCalc(site.place)

    val instants =
      List.unfold(start)(prev =>
        prev.plus(every).some.filter(_.isBefore(end)).map(i => (i, i))
      ) :+ end

    instants.map { i =>
      (i, calc.calculate(coordsForInstant(i), i, true))
    }
