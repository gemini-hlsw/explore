// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import crystal.react.*
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA

case class AsyncAction[M, K, A](
  asyncGet:  DefaultA[(K, A)],
  getter:    K => M => A,
  setter:    K => A => M => M,
  onSet:     K => (M, A) => DefaultA[Unit],
  onRestore: K => (M, A) => DefaultA[Unit]
):
  def apply(undoSetter: UndoSetter[M]): DefaultA[(K, A)] =
    asyncGet.flatMap: (k, a) =>
      undoSetter.set(getter(k), setter(k), onSet(k), onRestore(k))(a).to[DefaultA].as((k, a))
