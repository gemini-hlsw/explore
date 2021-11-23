package explore.itc

import cats._
import cats.effect._
import cats.effect.std.Queue
import cats.effect.syntax.all._
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.common.ITCQueriesGQL._
import explore.model.AirMassRange
import explore.model.ConstraintSet
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
