// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS

/** Undo controls */
trait Undoer:
  def undo: DefaultS[Unit]
  def redo: DefaultS[Unit]
  def isUndoEmpty: Boolean
  def isRedoEmpty: Boolean
  def working: Boolean
