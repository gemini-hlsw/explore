// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Applicative
import cats.Monoid
import cats.effect.Deferred
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import crystal.react.*
import lucuma.react.primereact.Message
import lucuma.react.primereact.MessageItem
import lucuma.react.primereact.ToastRef
import lucuma.ui.syntax.toast.*

class ToastCtx[F[_]: Sync](toastRef: Deferred[F, ToastRef]):
  def showToast(
    text:     String,
    severity: Message.Severity = Message.Severity.Info,
    sticky:   Boolean = false
  ): F[Unit] =
    toastRef.tryGet.flatMap:
      _.map:
        _.show(text, severity, sticky).to[F]
      .getOrElse:
        Applicative[F].unit

  def showToast(messages: MessageItem*): F[Unit] =
    toastRef.tryGet.flatMap:
      _.map:
        _.show(messages*).to[F]
      .getOrElse:
        Applicative[F].unit

  def replaceToast(message: MessageItem): F[Unit] =
    toastRef.tryGet.flatMap:
      _.map:
        _.remove(message).to[F]
      .getOrElse:
        Applicative[F].unit

  def removeToast(message: MessageItem): F[Unit] =
    toastRef.tryGet.flatMap:
      _.map:
        _.remove(message).to[F]
      .getOrElse:
        Applicative[F].unit

  def clear(): F[Unit] =
    toastRef.tryGet.flatMap:
      _.map:
        _.clear().to[F]
      .getOrElse:
        Applicative[F].unit

  def showToastDuring(
    text:         String,
    completeText: Option[String] = none,
    errorText:    Option[String] = none
  )(using
    UUIDGen[F],
    Monoid[F[Unit]]
  ): Resource[F, Unit] =
    Resource
      .eval(toastRef.tryGet)
      .flatMap:
        _.map:
          _.showDuring(text, completeText, errorText)
        .getOrElse:
          Resource.unit[F]

object ToastCtx:
  def apply[F[_]](using ToastCtx[F]): ToastCtx[F] = summon[ToastCtx[F]]
