// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package japgolly.webapputil

package object indexeddb {

  type TxnDslRO = TxnDsl.RO.type
  type TxnDslRW = TxnDsl.RW.type

  @inline def TxnDslRO: TxnDslRO = TxnDsl.RO
  @inline def TxnDslRW: TxnDslRW = TxnDsl.RW

}
