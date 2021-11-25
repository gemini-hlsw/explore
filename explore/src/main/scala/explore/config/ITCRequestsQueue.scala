package explore.itc

import cats._
import cats.effect._
import cats.effect.std.Queue
import cats.effect.syntax.all._
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.common.ITCQueriesGQL._
import explore.model.AirMassRange
import explore.model.ConstraintSet
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.SpectroscopyModeRow
import explore.schemas.ITC
import explore.schemas.itcschema.implicits._
import fs2.Stream
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.enum._
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Wavelength
import lucuma.core.model.Magnitude
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._

final case class ITCRequest[F[_]](
  mode:          InstrumentModes,
  wavelength:    Wavelength,
  signalToNoise: PosBigDecimal,
  callback:      SpectroscopyITCQuery.Data => F[Unit]
)

final case class ITCRequestsQueue[F[_]](f: Queue[F, Option[ITCRequest[F]]]) {
  def run(implicit
    E: Temporal[F],
    L: Logger[F],
    T: TransactionalClient[F, ITC]
  ): F[Unit] =
    Stream
      .fromQueueNoneTerminated(f)
      .meteredStartImmediately(500.milliseconds) // Limit how fasts requests are created
      .evalMap(f => ITCRequestsQueue.doRequest(f))
      .compile
      .drain

  def add(itc: ITCRequest[F]): F[Unit] =
    f.offer(itc.some)
}

object ITCRequestsQueue {
  type RequestsList[F[_]] = List[ITCRequest[F]] Refined MaxSize[4]

  implicit val itcRequestsQueueReuse: Reusability[ITCRequestsQueue[IO]] = Reusability.always

  def build[F[_]: Async: Logger: TransactionalClient[*[_], ITC]](
    set: Option[ITCRequestsQueue[F]] => F[Unit]
  ): F[Unit] =
    for {
      queue <- Queue.unbounded[F, Option[ITCRequest[F]]]
      rq     = ITCRequestsQueue(queue)
      _     <- rq.run.start
      r     <- set(rq.some)
    } yield r

  implicit class Row2Modes(val r: SpectroscopyModeRow) extends AnyVal {
    def toMode: Option[InstrumentModes] = r.instrument match {
      case GmosNorthSpectroscopyRow(d, f, fi) =>
        (new InstrumentModes(
          new GmosNITCInput(d, f, Input.orIgnore(fi)).assign
        )).some
      case _                                  => none
    }
  }

  def queryItc[F[_]: Applicative](
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    modes:         List[SpectroscopyModeRow],
    cache:         ItcResultsCache,
    cacheUpdate:   (ItcResultsCache => ItcResultsCache) => F[Unit],
    queue:         ITCRequestsQueue[F]
  ): F[Unit] =
    modes
      .map(_.instrument)
      // Only handle known modes
      .collect { case m: GmosNorthSpectroscopyRow =>
        InstrumentModes(m.toGmosNITCInput)
      }
      // Discard values in the cache
      .filterNot { case m =>
        cache.cache.contains((wavelength, signalToNoise, m))
      }
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      .traverse_ { m =>
        queue.add(
          ITCRequest[F](
            m,
            wavelength,
            signalToNoise,
            { x =>
              // Convert to usable types and update the cache
              val update = x.spectroscopy.flatMap(_.results).map { r =>
                val im = InstrumentModes(
                  GmosNITCInput(r.mode.params.disperser,
                                r.mode.params.fpu,
                                r.mode.params.filter.orIgnore
                  ).assign
                )
                val m  = r.itc match {
                  case ItcError(m)      => ItcQueryProblems.GenericError(m).leftNec
                  case ItcSuccess(e, t) => ItcResult.Result(t.microseconds.microseconds, e).rightNec
                }
                (wavelength, signalToNoise, im) -> m
              }
              cacheUpdate(
                ItcResultsCache.updateCount.modify(_ + 1) >>> ItcResultsCache.cache
                  .modify(_ ++ update)
              )
            }
          )
        )
      }

  def doRequest[F[_]: FlatMap: Logger: TransactionalClient[*[_], ITC]](
    request: ITCRequest[F]
  ): F[Unit] =
    Logger[F].info(s"ITC request for mode ${request.mode}") *>
      SpectroscopyITCQuery
        .query(
          ITCSpectroscopyInput(
            request.wavelength.toITCInput,
            request.signalToNoise,
            // TODO Link target info to explore
            SpatialProfile.PointSource,
            SpectralDistribution.Library(StellarLibrarySpectrum.A0I.asLeft),
            Magnitude(MagnitudeValue(20), MagnitudeBand.I, none, MagnitudeSystem.Vega).toITCInput,
            BigDecimal(0.1),
            // TODO Link constraints info to explore
            ConstraintSet(
              ImageQuality.PointSix,
              CloudExtinction.PointFive,
              SkyBackground.Dark,
              WaterVapor.Median,
              AirMassRange(
                AirMassRange.DefaultMin,
                AirMassRange.DefaultMax
              )
            ),
            List(request.mode.assign)
          ).assign
        )
        .flatMap(request.callback)
}
