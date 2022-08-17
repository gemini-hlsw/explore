// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.Eval
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.unsafe.implicits.given
import cats.syntax.all.given
import explore.events.PlotMessage
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.skycalc.solver.ElevationSolver
import lucuma.core.math.skycalc.solver.Samples
import lucuma.core.model.Semester
import lucuma.core.model.TwilightBoundedNight
import lucuma.core.syntax.boundedInterval.given
import lucuma.core.syntax.time.given
import org.typelevel.cats.time.given
import org.typelevel.log4cats.Logger
import spire.math.Bounded

import java.time.Duration
import java.time.Instant
import java.time.LocalTime
import scala.ContextFunction1
import scala.collection.immutable.TreeMap
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("PlotServer", moduleID = "exploreworkers")
object PlotServer extends WorkerServer[IO, PlotMessage.Request] {

  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val SampleRate: Duration = Duration.ofMinutes(1)

  private val MinTargetElevation = Declination.fromDoubleDegrees(30.0).get

  private final case class SemesterPlotCalc(semester: Semester, site: Site) {

    def samples(
      coordsForInstant: Instant => Coordinates,
      dayRate:          Long
    ): Samples[Duration] = {
      val results = Samples
        .atFixedRate(
          Bounded.unsafeOpenUpper(
            semester.start.atSite(site).toInstant,
            semester.end.atSite(site).toInstant
          ),
          SampleRate
        )(coordsForInstant)
        .toSkyCalResultsAt(site.place)

      val targetVisible = ElevationSolver(MinTargetElevation, Declination.Max).solve(results) _

      Samples.fromMap(
        Iterator
          .iterate(semester.start.localDate)(_.plusDays(dayRate))
          .takeWhile(_ <= semester.end.localDate)
          .map { date =>
            val instant = date.atTime(LocalTime.MIDNIGHT).atZone(site.timezone).toInstant
            instant -> Eval.later {
              val night     = TwilightBoundedNight
                .fromTwilightTypeAndSiteAndLocalDateUnsafe(TwilightType.Nautical, site, date)
              val intervals = targetVisible(night.interval)
              intervals.duration
            }
          }
          .to(TreeMap)
      )
    }
  }

  override protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] = IO { invocation =>
    invocation.data match {
      case PlotMessage.RequestSemesterSidereal(semester, site, coords, dayRate) =>
        fs2.Stream
          .fromIterator[IO](
            SemesterPlotCalc(semester, site).samples(_ => coords, dayRate).iterator,
            1
          )
          .evalMap { case (instant, visibilityDuration) =>
            invocation.respond(
              PlotMessage.SemesterPoint(instant.toEpochMilli, visibilityDuration.toMillis)
            )
          }
          .compile
          .drain
    }
  }

}
