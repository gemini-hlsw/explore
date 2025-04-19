// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits.given
import cats.syntax.all.given
import explore.events.PlotMessage
import explore.model.boopickle.CommonPicklers.given
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.skycalc.SkyCalcResults
import lucuma.core.math.skycalc.solver.ElevationSolver
import lucuma.core.math.skycalc.solver.Samples
import lucuma.core.model.Semester
import lucuma.core.model.TwilightBoundedNight
import lucuma.core.syntax.time.*
import org.scalajs.dom
import org.typelevel.cats.time.given
import org.typelevel.log4cats.Logger
import spire.math.extras.interval.IntervalSeq

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.LocalTime
import scala.ContextFunction1
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

import scalajs.js

@JSExportTopLevel("PlotServer", moduleID = "exploreworkers")
object PlotServer extends WorkerServer[IO, PlotMessage.Request] {

  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val SampleRate: Duration = Duration.ofMinutes(1)

  private val MinTargetElevation = Declination.fromDoubleDegrees(30.0).get

  private val CacheRetention: Duration = Duration.ofDays(30)

  private case class SemesterPlotCalc(semester: Semester, site: Site, cache: Cache[IO]) {

    def siderealVisibility(
      skyCalcSamples: Samples[SkyCalcResults]
    ): Cacheable[IO, (Site, LocalDate, Coordinates), (Instant, Duration)] = {
      val targetVisible: BoundedInterval[Instant] => IntervalSeq[Instant] =
        ElevationSolver(MinTargetElevation, Declination.Max).solve(skyCalcSamples)(_)

      Cacheable(
        CacheName("siderealVisibility"),
        CacheVersion(5),
        (site, date, _) =>
          IO {
            val instant = date.atTime(LocalTime.MIDNIGHT).atZone(site.timezone).toInstant
            instant -> {
              val night     = TwilightBoundedNight
                .fromTwilightTypeAndSiteAndLocalDateUnsafe(TwilightType.Nautical, site, date)
              val intervals = targetVisible(night.interval)
              intervals.duration
            }
          }
      )
    }

    def siderealSamples(coords: Coordinates, dayRate: Long): fs2.Stream[IO, (Instant, Duration)] = {
      val skyCalcSamples = Samples
        .atFixedRate(
          BoundedInterval.unsafeOpenUpper(
            semester.start.atSite(site).toInstant,
            semester.end.atSite(site).toInstant
          ),
          SampleRate
        )(_ => coords)
        .toSkyCalResultsAt(site.place)

      val visibilityCalc = siderealVisibility(skyCalcSamples)

      fs2.Stream
        .fromIterator[IO](
          Iterator
            .iterate(semester.start.localDate)(_.plusDays(dayRate))
            .takeWhile(_ <= semester.end.localDate),
          1
        )
        .evalMap { date =>
          cache
            .eval(visibilityCalc)
            .apply(site, date, coords)
        }
    }
  }

  override protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for
      self  <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache <- Cache.withIDB[IO](self.indexedDB.toOption, "explore-plots")
      _     <- cache.evict(CacheRetention).start
    yield invocation =>
      invocation.data match {
        case PlotMessage.CleanCache                                               =>
          cache.clear *> invocation.respond(())
        case PlotMessage.RequestSemesterSidereal(semester, site, coords, dayRate) =>
          SemesterPlotCalc(semester, site, cache)
            .siderealSamples(coords, dayRate)
            .evalMap { case (instant, visibilityDuration) =>
              invocation.respond(
                PlotMessage.SemesterPoint(instant.toEpochMilli, visibilityDuration.toMillis)
              )
            }
            .compile
            .drain
      }

}
