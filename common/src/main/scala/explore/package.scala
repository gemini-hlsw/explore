// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import cats.effect.IO
import crystal.ViewF
import crystal.ViewOptF
import explore.model.AppContext

package explore {

  import cats.effect.SyncIO
  import explore.undo.UndoContext
  import explore.undo.UndoSetter
  trait ShorthandTypes {
    type AppContextIO = AppContext[IO]
    type View[A]      = ViewF[SyncIO, A]
    type ViewOpt[A]   = ViewOptF[SyncIO, A]
    type UndoCtx[A]   = UndoContext[SyncIO, IO, A]
    type UndoSet[A]   = UndoSetter[SyncIO, IO, A]
  }

}

package object explore extends explore.ShorthandTypes
