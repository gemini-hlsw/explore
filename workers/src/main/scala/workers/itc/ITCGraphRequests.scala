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
import explore.model.itc._
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
import queries.schemas.itc.implicits.*

import java.util.UUID
import scala.concurrent.duration._
import explore.modes.InstrumentRow

object ITCGraphRequests {
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
    targets:       NonEmptyList[ItcTarget],
    mode:          InstrumentRow
    // callback:      Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcGraphResult]] => F[Unit]
  )(using Monoid[F[Unit]], TransactionalClient[F, ITC]): F[Unit] = {
    // def itcResults(r: List[ItcGraphResults]): List[EitherNec[ItcQueryProblems, ItcGraphResult]] =
    //   // Convert to usable types
    //   r.map(x =>
    //     x.spectroscopyGraph match {
    //       case ItcError(m)      => ItcQueryProblems.GenericError(m).leftNec
    //       case ItcSuccess(e, t) => ItcGraphResult.Result("title").rightNec
    //     }
    //   )
    //
    // Find the magnitude closest to the requested wavelength
    def selectedBrightness(
      sourceProfile: SourceProfile,
      wavelength:    Wavelength
    ): Option[Band] =
      SourceProfile.integratedBandNormalizedSpectralDefinition
        .andThen(
          SpectralDefinition.BandNormalized.brightnesses[Integrated]
        )
        .getOption(sourceProfile)
        .orElse {
          SourceProfile.surfaceBandNormalizedSpectralDefinition
            .andThen(
              SpectralDefinition.BandNormalized.brightnesses[Surface]
            )
            .getOption(sourceProfile)
        }
        .map(_.keys)
        .traverse(
          _.minByOption((band: Band) =>
            (band.center.toPicometers.value.value - wavelength.toPicometers.value.value).abs
          )
        )
        .collect { case Some(b) => b }

    // def doRequest(
    //   request:  ItcRequestParams,
    //   callback: List[ItcResults] => F[Unit]
    // ): F[Unit] =
    //   Logger[F].debug(
    //     s"ITC: Request for mode ${request.mode} and target count: ${request.target.length}"
    //   ) *>
    //     request.target
    //       .fproduct(t => selectedBrightness(t.profile, request.wavelength))
    //       .collect { case (t, Some(brightness)) =>
    //         SpectroscopyGraphITCQuery
    //           .query(
    //             SpectroscopyGraphModeInput(
    //               request.wavelength.toInput,
    //               request.signalToNoise,
    //               t.profile.toInput,
    //               brightness,
    //               t.rv.toITCInput,
    //               request.constraints,
    //               request.mode.toITCInput.getOrElse(null)
    //             ).assign
    //           )
    //       }
    //       .void
    // .parSequence
    // .flatTap { r =>
    //   val prefix = s"ITC: Result for mode ${request.mode}:"
    //   itcResults(r).traverse(_ match {
    //     case Left(errors)                                     =>
    //       Logger[F].error(s"$prefix ERRORS: $errors")
    //     case Right(ItcResult.Result(exposureTime, exposures)) =>
    //       Logger[F].debug(s"$prefix $exposures x ${exposureTime.toSeconds}")
    //     case Right(other)                                     =>
    //       Logger[F].debug(s"$prefix $other")
    //   })
    // }
    // .flatMap(callback)

    val itcRowsParams = mode match // Only handle known modes
      case m: GmosNorthSpectroscopyRow =>
        ItcRequestParams(wavelength, signalToNoise, constraints, targets, m).some
      case m: GmosSouthSpectroscopyRow =>
        ItcRequestParams(wavelength, signalToNoise, constraints, targets, m).some
      case _                           => none

      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
    itcRowsParams.map { request =>
      Logger[F].debug(
        s"ITC: Request for mode ${request.mode} and target count: ${request.target.length}"
      ) *>
        request.target
          .fproduct(t => selectedBrightness(t.profile, request.wavelength))
          .collect { case (t, Some(brightness)) =>
            SpectroscopyGraphITCQuery
              .query(
                SpectroscopyGraphModeInput(
                  request.wavelength.toInput,
                  request.signalToNoise,
                  t.profile.toInput,
                  brightness,
                  t.rv.toITCInput,
                  request.constraints,
                  request.mode.toITCInput.getOrElse(null)
                ).assign
              )
          }
          .sequence
          .void
    }.orEmpty
    // doRequest(
    //   itcRowsParams,
    //   { r =>
    //     // Convert to usable types and update the cache
    //     val update: Option[EitherNec[ItcQueryProblems, ItcResult]] =
    //       // There maybe multiple targets, take the one with the max time
    //       itcResults(r) match {
    //         case Nil => none
    //         case l   =>
    //           l.maxBy {
    //             case Right(ItcResult.Result(exposureTime, _)) => exposureTime.toMicros
    //             case _                                        => Long.MinValue
    //           }.some
    //       }
    //     // Send the request to the front
    //     update
    //       .map(r => callback(Map(params -> r)))
    //       .getOrElse(Applicative[F].unit)
    //
    //   }
    // )
  }

}
