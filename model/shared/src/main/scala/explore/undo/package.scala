// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

package object undo {
  type Mod[F[_], M] = (M => M) => F[Unit]

  type UndoStack[F[_], M] = List[Restorer[F, M]]
}
