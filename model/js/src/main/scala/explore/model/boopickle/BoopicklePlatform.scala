// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic._
import cats.effect._
import cats.syntax.all._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

trait BoopicklePlatform {

  def asTransferable[A: Pickler](value: A): Int8Array =
    Pickle.intoBytes(value).typedArray()

  def postAsTransferable[F[_]: Sync, A: Pickler](self: dom.DedicatedWorkerGlobalScope, value: A) =
    Sync[F].delay {
      val arr = asTransferable(value)
      self.postMessage(arr, js.Array(arr.buffer: dom.Transferable))
    }

  def fromTransferable[A: Pickler](buffer: Int8Array): Either[Throwable, A] =
    Either
      .catchNonFatal(Unpickle[A].fromBytes(TypedArrayBuffer.wrap(buffer)))

  def decodeFromTransferable[A: Pickler](m: dom.MessageEvent): Option[A] =
    m.data match {
      case e: Int8Array => fromTransferable[A](e).toOption
      case _            => none
    }
}

object Boopickle extends BoopicklePlatform
