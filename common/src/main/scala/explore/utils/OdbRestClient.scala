// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.effect.Async
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.AppConfig
import fs2.Stream
import fs2.text.utf8
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.model.Attachment
import lucuma.core.model.Program
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.http4s.headers.Authorization
import org.scalajs.dom

import scala.concurrent.duration.*
import scala.util.control.NoStackTrace

trait OdbRestClient[F[_]] {
  // Allows us to have a reuse - needed for memoization, etc.
  def authToken: NonEmptyString

  def getAttachment(attachmentId: Attachment.Id): F[Stream[F, Byte]]

  def getAttachmentUrl(attachmentId: Attachment.Id): F[String]

  def insertAttachment(
    programId:      Program.Id,
    attachmentType: AttachmentType,
    fileName:       NonEmptyString,
    description:    Option[NonEmptyString],
    data:           Stream[F, Byte]
  ): F[Attachment.Id]

  def updateAttachment(
    attachmentId: Attachment.Id,
    fileName:     NonEmptyString,
    description:  Option[NonEmptyString],
    data:         Stream[F, Byte]
  ): F[Unit]

  def deleteAttachment(attachmentId: Attachment.Id): F[Unit]
}

object OdbRestClient {
  def apply[F[_]: Async](env: ExecutionEnvironment, authToken: NonEmptyString): OdbRestClient[F] = {

    val authHeader = Headers(
      Authorization(Credentials.Token(AuthScheme.Bearer, authToken.value))
    )

    given QueryParamEncoder[NonEmptyString] = QueryParamEncoder[String].contramap(_.value)

    def getURI: F[Uri] =
      AppConfig
        .fetchConfig(
          env,
          FetchClientBuilder[F]
            .withRequestTimeout(5.seconds)
            .withCache(dom.RequestCache.`no-store`)
            .create
        )
        .map(_.odbRestURI / "attachment")

    def client: Client[F] = FetchClientBuilder[F].withRequestTimeout(60.seconds).create

    def mkError[A](msg: String): F[A] = Async[F].raiseError(new Exception(msg) with NoStackTrace)

    def runRequest(action: String)(f: Uri => Request[F]): Resource[F, Response[F]] =
      val resource = for {
        uri <- Resource.eval(getURI)
        res <- client.run(f(uri))
      } yield res
      resource.evalMap(res =>
        if (res.status === Status.Ok) Async[F].pure(res)
        else res.error(action)
      )

    extension (response: Response[F])
      def getBody: F[String]             = response.body.through(utf8.decode).compile.string
      def error[A](action: String): F[A] =
        getBody.flatMap { b =>
          val bodyMsg = if (b.isEmpty) response.status.reason else b
          mkError(s"Error $action: $bodyMsg")
        }

    new OdbRestClient[F] {
      val authToken: NonEmptyString = authToken

      def getAttachment(
        attachmentId: Attachment.Id
      ): F[Stream[F, Byte]] =
        runRequest("Getting Attachment")(baseUri =>
          Request[F](
            method = Method.GET,
            uri = baseUri / attachmentId.show,
            headers = authHeader
          )
        ).use(r => Async[F].pure(r.body))

      def getAttachmentUrl(attachmentId: Attachment.Id): F[String] =
        runRequest("Getting URL")(baseUri =>
          Request[F](
            method = Method.GET,
            uri = baseUri / "url" / attachmentId.show,
            headers = authHeader
          )
        ).use(
          _.getBody
        )

      def insertAttachment(
        programId:      Program.Id,
        attachmentType: AttachmentType,
        fileName:       NonEmptyString,
        description:    Option[NonEmptyString],
        data:           Stream[F, Byte]
      ): F[Attachment.Id] =
        runRequest("Adding Attachment") { baseUri =>
          val uri = baseUri
            .withQueryParam("programId", programId.show)
            .withQueryParam("fileName", fileName)
            .withQueryParam("attachmentType", attachmentType.tag)
            .withOptionQueryParam("description", description)
          Request[F](
            method = Method.POST,
            uri = uri,
            headers = authHeader,
            body = data
          )
        }.use(
          _.getBody.flatMap(
            Attachment.Id.parse(_).fold(mkError("Invalid Attachment Id returned"))(Async[F].pure)
          )
        )

      def updateAttachment(
        attachmentId: Attachment.Id,
        fileName:     NonEmptyString,
        description:  Option[NonEmptyString],
        data:         Stream[F, Byte]
      ): F[Unit] =
        runRequest("Updating Attachment") { baseUri =>
          val uri = (baseUri / attachmentId.show)
            .withQueryParam("fileName", fileName)
            .withOptionQueryParam("description", description)
          Request[F](
            method = Method.PUT,
            uri = uri,
            headers = authHeader,
            body = data
          )
        }.use(_ => Async[F].unit)

      def deleteAttachment(attachmentId: Attachment.Id): F[Unit] =
        runRequest("Deleting Attachment") { baseUri =>
          val uri = baseUri / attachmentId.show
          Request[F](
            method = Method.DELETE,
            uri = uri,
            headers = authHeader
          )
        }.use(_ => Async[F].unit)
    }
  }
}
