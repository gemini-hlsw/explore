// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package japgolly.webapputil.indexeddb

import org.scalajs.dom.IDBKey

final class IndexedDbKey private (val asJs: IDBKey) extends AnyVal {
  @inline def value = asJs.asInstanceOf[IndexedDbKey.Typed]
}

object IndexedDbKey {

  // https://w3c.github.io/IndexedDB/#key-construct
  // A key has an associated type which is one of: number, date, string, binary, or array.

  type Typed = String | Double

  @inline def apply(t: Typed): IndexedDbKey =
    fromJs(t)

  def fromJs(k: IDBKey): IndexedDbKey =
    new IndexedDbKey(k)
}
