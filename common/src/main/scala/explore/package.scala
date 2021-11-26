// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import cats.effect.IO
import explore.model.AppContext

package explore {

  trait ShorthandTypes {
    type AppContextIO = AppContext[IO]
  }
}

package object explore extends explore.ShorthandTypes
