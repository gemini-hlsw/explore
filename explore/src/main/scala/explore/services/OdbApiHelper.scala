// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.ResponseException
import clue.model.GraphQLError
import clue.model.GraphQLResponse
import clue.model.GraphQLResponse.*
import lucuma.odb.data.OdbError

trait OdbApiHelper[F[_]: MonadThrow](resetCache: String => F[Unit]):
  private case class OdbAdaptedError[D](message: String, cause: ResponseException[D])
      extends RuntimeException(message, cause)

  extension (graphQlError: GraphQLError)
    private def asOdbError: Option[OdbError] =
      for
        extensions <- graphQlError.extensions
        json       <- extensions.get(OdbError.Key)
        odbError   <- json.as[OdbError].toOption
      yield odbError

  extension [A](fa: F[A])
    private def processOdbErrors: F[A] =
      fa.adaptError:
        case e @ ResponseException(errors, data) =>
          errors
            .traverse(_.asOdbError)
            .map(odbErrors => OdbAdaptedError(odbErrors.map(_.message).toList.mkString("\n"), e))
            .getOrElse(e)

    protected def resetCacheOnError: F[A] =
      fa.onError: e =>
        resetCache(e.getMessage)

  extension [D](fa: F[GraphQLResponse[D]])
    protected def processErrors: F[D] =
      fa.raiseGraphQLErrors.processOdbErrors.resetCacheOnError

    protected def processNoDataErrors: F[D] =
      fa.raiseGraphQLErrorsOnNoData.processOdbErrors.resetCacheOnError

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
