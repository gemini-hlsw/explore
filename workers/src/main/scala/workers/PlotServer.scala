// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.Eval
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.unsafe.implicits.given
import cats.syntax.all.given
import explore.events.PlotMessage
import explore.model.boopickle.CommonPicklers.given
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Place
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.math.skycalc.ImprovedSkyCalc.apply
import lucuma.core.math.skycalc.SkyCalcResults
import lucuma.core.math.skycalc.solver.ElevationSolver
import lucuma.core.math.skycalc.solver.Samples
import lucuma.core.model.Semester
import lucuma.core.model.TwilightBoundedNight
import lucuma.core.syntax.boundedInterval.given
import lucuma.core.syntax.time.given
import lucuma.core.util.Enumerated
import org.scalajs.dom
import org.typelevel.cats.time.given
import org.typelevel.log4cats.Logger
import spire.math.Bounded
import spire.math.extras.interval.IntervalSeq

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.LocalTime
import scala.ContextFunction1
import scala.collection.immutable.TreeMap
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

import scalajs.js

@JSExportTopLevel("PlotServer", moduleID = "exploreworkers")
object PlotServer extends WorkerServer[IO, PlotMessage.Request] {

  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val SampleRate: Duration = Duration.ofMinutes(1)

  private val MinTargetElevation = Declination.fromDoubleDegrees(30.0).get

  private val skyCalcForSite: Map[Site, ImprovedSkyCalc] =
    Enumerated[Site].all.map(s => s -> ImprovedSkyCalc(s.place)).toMap

  private val CacheVersion: Int = 1

  private given Pickler[SkyCalcResults] = generatePickler

  private final case class SemesterPlotCalc(semester: Semester, site: Site, cache: Cache[IO])(using
    Logger[IO]
  ) {

    def siderealVisibility(
      skyCalcSamples: Samples[SkyCalcResults]
    ): Cacheable[IO, (Site, LocalDate, Coordinates), (Instant, Duration)] = {
      val targetVisible: Bounded[Instant] => IntervalSeq[Instant] =
        ElevationSolver(MinTargetElevation, Declination.Max).solve(skyCalcSamples) _

      Cacheable(
        "siderealVisibility",
        CacheVersion,
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
          Bounded.unsafeOpenUpper(
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
      cache <- Cache.withJsCache[IO](self.caches.toOption, "explore-plots")
    // cache <- Cache.withIDB[IO](self.indexedDB.toOption, "explore-plots")
    yield invocation =>
      invocation.data match {
        case PlotMessage.RequestSemesterSidereal(semester, site, coords, dayRate) =>
          Logger[IO].info("HELLO!") >>
            SemesterPlotCalc(semester, site, cache)
              .siderealSamples(coords, dayRate)
              .evalMap { case (instant, visibilityDuration) =>
                invocation.respond(
                  PlotMessage.SemesterPoint(instant.toEpochMilli, visibilityDuration.toMillis)
                )
              }
              .compile
              .drain
              .handleErrorWith(t => Logger[IO].error(t)("ERROR!!!"))
      }

}
