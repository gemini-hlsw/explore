// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.all.*
import clue.ResponseException
import clue.model.GraphQLError
import clue.model.GraphQLResponse
import clue.syntax.*
import lucuma.odb.data.OdbError
import org.typelevel.log4cats.Logger

trait OdbApiHelper[F[_]: Sync: Logger](
  resetCache:       String => F[Unit],
  notifyFatalError: String => F[Unit]
):
  private case class OdbAdaptedError[D](message: String, cause: ResponseException[D])
      extends RuntimeException(message, cause)

  extension (graphQlError: GraphQLError)
    private def asOdbError: Option[OdbError] =
      for
        extensions <- graphQlError.extensions
        json       <- extensions(OdbError.Key)
        odbError   <- json.as[OdbError].toOption
      yield odbError

  private val adaptResponseException: PartialFunction[Throwable, Throwable] =
    case e @ ResponseException(errors, _) =>
      errors
        .traverse(_.asOdbError)
        .map(odbErrors => OdbAdaptedError(odbErrors.map(_.message).toList.mkString("\n"), e))
        .getOrElse(e)

  // we don't want to reset if there are unrecoverable errors like an incompatible schema
  private def shouldResetCache(error: Throwable): Boolean = error match {
    case ResponseException(errors, _) =>
      // This detects one particular case, there may be more.
      !errors.exists(_.message.startsWith("No field"))
    case _                            => true
  }

  extension [A](fa: F[A])
    private def adaptOdbErrors: F[A] =
      fa.adaptError(adaptResponseException)

    protected def resetCacheOnError: F[A] =
      fa.onError: e =>
        val doReset = shouldResetCache(e)
        Logger[F].error(e)(s"Error in ODB API call $doReset") >>
          resetCache(e.getMessage).whenA(doReset) >>
          notifyFatalError(e.getMessage).unlessA(doReset)

  extension [D](fa: F[GraphQLResponse[D]])
    protected def processErrors: F[D] =
      fa.raiseGraphQLErrors.adaptOdbErrors.resetCacheOnError

    protected def processNoDataErrors: F[D] =
      fa.raiseGraphQLErrorsOnNoData.adaptOdbErrors.resetCacheOnError

  protected def drain[A, Id, R](
    fetch:      Option[Id] => F[R],
    getList:    R => List[A],
    getHasMore: R => Boolean,
    getId:      A => Id
  ): F[List[A]] = {
    def go(id: Option[Id], accum: List[A]): F[List[A]] =
      fetch(id).flatMap(result =>
        val list = getList(result)
        if (getHasMore(result)) go(list.lastOption.map(getId), list)
        // Fetching with offset includes the offset, so .dropRight(1) ensures we don't include it twice.
        else (accum.dropRight(1) ++ list).pure[F]
      )

    go(none, List.empty)
  }

  extension [D](subscription: Resource[F, fs2.Stream[F, GraphQLResponse[D]]])
    protected def processErrors(logPrefix: String): Resource[F, fs2.Stream[F, D]] =
      subscription.raiseFirstNoDataError
        .handleGraphQLErrors: e =>
          val re: Throwable = adaptResponseException(e)
          Logger[F].error(re)(s"[$logPrefix] Error in subscription")
        .map:
          _.onError: e =>
            fs2.Stream.eval:
              Logger[F].error(e)(s"Error in ODB API call, resetting cache") >>
                resetCache(e.getMessage)
