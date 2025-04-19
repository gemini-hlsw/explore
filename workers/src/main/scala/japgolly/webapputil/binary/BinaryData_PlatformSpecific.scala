// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package japgolly.webapputil.binary

// **********
// *        *
// *   JS   *
// *        *
// **********

import org.scalajs.dom.Blob

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.Uint8Array

trait BinaryData_PlatformSpecific_Object { self: BinaryData.type =>

  def fromArrayBuffer(ab: ArrayBuffer): BinaryData =
    BinaryData.fromByteBuffer(BinaryJs.arrayBufferToByteBuffer(ab))

  def fromUint8Array(a: Uint8Array): BinaryData =
    fromArrayBuffer(BinaryJs.uint8ArrayToArrayBuffer(a))

  def unsafeFromArrayBuffer(ab: ArrayBuffer): BinaryData =
    BinaryData.unsafeFromByteBuffer(BinaryJs.arrayBufferToByteBuffer(ab))

  def unsafeFromUint8Array(a: Uint8Array): BinaryData =
    unsafeFromArrayBuffer(BinaryJs.uint8ArrayToArrayBuffer(a))
}

trait BinaryData_PlatformSpecific_Instance { self: BinaryData =>

  def toArrayBuffer: ArrayBuffer =
    BinaryJs.byteBufferToArrayBuffer(self.unsafeByteBuffer)

  def toUint8Array: Uint8Array =
    new Uint8Array(toArrayBuffer)

  def toBlob: Blob =
    BinaryJs.byteBufferToBlob(self.unsafeByteBuffer)

  def toNewJsArray: js.Array[Byte] =
    self.toNewArray.toJSArray

  def unsafeArrayBuffer: js.typedarray.ArrayBufferView =
    BinaryJs.byteBufferToInt8Array(self.unsafeByteBuffer)

  def unsafeUint8Array: Uint8Array =
    new Uint8Array(toArrayBuffer)

  def unsafeBlob: Blob =
    BinaryJs.byteBufferToBlob(self.unsafeByteBuffer)

  def unsafeJsArray: js.Array[Byte] =
    self.unsafeArray.toJSArray
}
