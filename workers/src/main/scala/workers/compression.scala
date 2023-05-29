// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers.compression

import workers.Pickled

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*

import scalajs.js.typedarray.*

extension (a: js.Array[Short])
  def toPickled: Pickled =
    val uia = new Uint8Array(a)
    val i8a = new Int8Array(uia.buffer, 0, uia.length)
    Pickled(i8a.toArray)

def compressBytes(p: Array[Byte]): Array[Byte] =
  encode_raw(p.toTypedArray).toArray

def decompressBytes(p: Pickled): Array[Byte] = {
  val m   = new Uint8Array(p.value.toJSArray.asInstanceOf[js.Array[Short]])
  val ia  = decode_raw(m)
  val i8a = new Int8Array(ia.buffer, 0, ia.length)
  i8a.toArray
}

@js.native
@JSImport("@cquiroz/wasm-flate", "deflate_encode_raw")
private def encode_raw(i: Int8Array): Int8Array = js.native

@js.native
@JSImport("@cquiroz/wasm-flate", "deflate_decode_raw")
private def decode_raw(i: Uint8Array): Uint8Array = js.native
