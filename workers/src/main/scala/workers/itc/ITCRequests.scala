// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats._
import cats.data._
import cats.effect._
import cats.effect.std.Semaphore
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.ViewF
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.Constants
import explore.model.Progress
import explore.model.itc.*
import explore.model.itc.math.*
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.SpectroscopyModeRow
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.common.ITCQueriesGQL._
import queries.schemas.ITC
import queries.schemas.itc.implicits._

import java.util.UUID
import scala.concurrent.duration._

object ITCRequests {
  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: Concurrent: Parallel, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f:  A => F[B]) =
    Semaphore[F](n).flatMap { s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))
    }

  def queryItc[F[_]: Concurrent: Parallel: Logger](
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    constraints:   ConstraintSet,
    targets:       ItcTarget,
    modes:         List[SpectroscopyModeRow],
    callback:      Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]] => F[Unit]
  )(using Monoid[F[Unit]], TransactionalClient[F, ITC]): F[Unit] = {
    def itcResults(r: ItcResults): List[EitherNec[ItcQueryProblems, ItcResult]] =
      // Convert to usable types
      r.spectroscopy.flatMap(_.results).map { r =>
        r.itc match {
          case ItcError(m)      => ItcQueryProblems.GenericError(m).leftNec
          case ItcSuccess(e, t) => ItcResult.Result(t.microseconds.microseconds, e).rightNec
        }
      }

    def doRequest(
      params:        ItcRequestParams,
      innerCallback: ItcResults => F[Unit]
    ): F[Unit] =
      Logger[F]
        .debug(
          s"ITC: Request for mode ${params.mode} and target count: ${params.target.name.value}"
        ) *> selectedBrightness(params.target.profile, params.wavelength)
        .map { brightness =>
          SpectroscopyITCQuery
            .query(
              SpectroscopyModeInput(
                params.wavelength.toInput,
                params.signalToNoise,
                params.target.profile.toInput,
                brightness,
                params.target.rv.toITCInput,
                params.constraints,
                params.mode.toITCInput.map(_.assign).toList
              ).assign
            )
            .flatTap { r =>
              val prefix = s"ITC: Result for mode ${params.mode}:"
              itcResults(r).traverse(_ match {
                case Left(errors)                                     =>
                  Logger[F].error(s"$prefix ERRORS: $errors")
                case Right(ItcResult.Result(exposureTime, exposures)) =>
                  Logger[F].debug(s"$prefix $exposures x ${exposureTime.toSeconds}")
                case Right(other)                                     =>
                  Logger[F].debug(s"$prefix $other")
              })
            }
            .flatMap(innerCallback)
            .handleErrorWith(e =>
              callback(
                Map(params -> ItcQueryProblems.GenericError("Error calling ITC service").leftNec)
              )
            )
        }
        .getOrElse(Applicative[F].unit)

    val itcRowsParams = modes
      .map(_.instrument)
      // Only handle known modes
      .collect {
        case m: GmosNorthSpectroscopyRow =>
          ItcRequestParams(wavelength, signalToNoise, constraints, targets, m)
        case m: GmosSouthSpectroscopyRow =>
          ItcRequestParams(wavelength, signalToNoise, constraints, targets, m)
      }

    parTraverseN(
      Constants.MaxConcurrentItcRequests.toLong,
      itcRowsParams.reverse
    ) { params =>
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      doRequest(
        params,
        { r =>
          // Convert to usable types and update the cache
          val update: Option[EitherNec[ItcQueryProblems, ItcResult]] =
            // There maybe multiple targets, take the one with the max time
            itcResults(r) match {
              case Nil => none
              case l   =>
                l.maxBy {
                  case Right(ItcResult.Result(exposureTime, _)) => exposureTime.toMicros
                  case _                                        => Long.MinValue
                }.some
            }
          // Send the request to the front
          update
            .map(r => callback(Map(params -> r)))
            .getOrElse(Applicative[F].unit)

        }
      )
    }.void
  }

}
