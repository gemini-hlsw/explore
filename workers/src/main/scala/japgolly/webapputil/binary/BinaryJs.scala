// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package japgolly.webapputil.binary

import org.scalajs.dom.Blob
import org.scalajs.dom.FileReader
import org.scalajs.dom.window

import java.nio.ByteBuffer
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.*
import scala.scalajs.js.typedarray.TypedArrayBufferOps.*

object BinaryJs extends BinaryJs

trait BinaryJs {

  final def arrayBufferToBlob(a: ArrayBuffer): Blob =
    new Blob(js.Array(a))

  @inline final def arrayBufferToByteBuffer(a: ArrayBuffer): ByteBuffer =
    TypedArrayBuffer.wrap(a)

  final def base64ToByteBuffer(base64: String): ByteBuffer = {
    val binstr = window.atob(base64)
    val buf    = new Int8Array(binstr.length)
    var i      = 0
    binstr.foreach { ch =>
      buf(i) = ch.toByte
      i += 1
    }
    TypedArrayBuffer.wrap(buf)
  }

  final def blobToArrayBuffer(blob: Blob): ArrayBuffer = {
    var arrayBuffer: ArrayBuffer = null
    val fileReader               = new FileReader()
    fileReader.onload = e =>
      arrayBuffer = e.target.asInstanceOf[js.Dynamic].result.asInstanceOf[ArrayBuffer]
    fileReader.readAsArrayBuffer(blob)
    assert(arrayBuffer != null)
    arrayBuffer
  }

  final def byteBufferToArrayBuffer(bb: ByteBuffer): ArrayBuffer =
    int8ArrayToArrayBuffer(byteBufferToInt8Array(bb))

  final def byteBufferToBlob(bb: ByteBuffer): Blob =
    arrayBufferToBlob(byteBufferToArrayBuffer(bb))

  final def byteBufferToInt8Array(bb: ByteBuffer): Int8Array = {
    val limit = bb.limit()
    if (bb.hasTypedArray())
      bb.typedArray()
    else if (bb.hasArray) {
      var array  = bb.array()
      val offset = bb.arrayOffset()
      if (limit != array.length)
        array = array.slice(offset, offset + limit)
      new Int8Array(array.toJSArray)
    } else {
      val array = BinaryData.unsafeFromByteBuffer(bb).unsafeJsArray
      new Int8Array(array)
    }
  }

  final def int8ArrayToArrayBuffer(v: Int8Array): ArrayBuffer =
    arrayBufferViewToArrayBuffer(v)

  final def uint8ArrayToArrayBuffer(v: Uint8Array): ArrayBuffer =
    arrayBufferViewToArrayBuffer(v)

  final private def arrayBufferViewToArrayBuffer(v: ArrayBufferView): ArrayBuffer = {
    val off = v.byteOffset
    val len = v.byteLength
    if (len == v.buffer.byteLength)
      v.buffer
    else
      v.buffer.slice(off, off + len)
  }

}
