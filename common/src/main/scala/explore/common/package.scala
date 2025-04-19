// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import crystal.react.ReuseView
import crystal.react.reuse.Reuse
import explore.undo.UndoSetter
import japgolly.scalajs.react.util.DefaultEffects.Async as DefaultA
import monocle.Lens
import monocle.Optional
import monocle.Prism
import org.typelevel.log4cats.Logger

package object common {
  type Aligner[A, T] = AlignerF[DefaultA, A, T]

  object Aligner {
    def apply[A, T](
      undoCtx:         UndoSetter[A],
      remoteBaseInput: T,
      onMod:           T => DefaultA[Unit]
    ): Aligner[A, T] =
      AlignerF(undoCtx, remoteBaseInput, onMod)
  }

  type ReuseAligner[A, T] = Reuse[Aligner[A, T]]

  extension [A, T](self: ReuseAligner[A, T])
    def get: A = self.value.get

    def viewMod(toInput: A => T => T)(implicit
      logger: Logger[DefaultA]
    ): ReuseView[A] = self.map(_.viewMod(toInput))

    def view(toInput: A => T)(implicit
      logger: Logger[DefaultA]
    ): ReuseView[A] = self.map(_.view(toInput))

    def zoom[B, S](
      modelGet:  A => B,
      modelMod:  (B => B) => A => A,
      remoteMod: (S => S) => T => T
    ): ReuseAligner[B, S] =
      self.map(_.zoom(modelGet, modelMod, remoteMod))

    /** Drill-down specifying model `Lens` and delta structure modification function. */
    def zoom[B, S](
      lens:      Lens[A, B],
      remoteMod: (S => S) => T => T
    ): ReuseAligner[B, S] =
      zoom(lens.get, lens.modify, remoteMod)

    def zoomOpt[B, S](
      modelGetOpt: A => Option[B],
      modelMod:    (B => B) => A => A,
      remoteMod:   (S => S) => T => T
    ): Option[ReuseAligner[B, S]] =
      modelGetOpt(get).map(_ => zoom(modelGetOpt.andThen(_.get), modelMod, remoteMod))

    def zoomOpt[B, S](
      prism:     Prism[A, B],
      remoteMod: (S => S) => T => T
    ): Option[ReuseAligner[B, S]] =
      zoomOpt(prism.getOption, prism.modify, remoteMod)

    def zoomOpt[B, S](
      optional:  Optional[A, B],
      remoteMod: (S => S) => T => T
    ): Option[ReuseAligner[B, S]] =
      zoomOpt(optional.getOption, optional.modify, remoteMod)
}
