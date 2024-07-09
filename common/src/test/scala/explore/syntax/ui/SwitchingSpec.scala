// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.syntax.ui

import cats.effect.IO
import cats.effect.Ref
import crystal.ViewF

class SwitchingSpec extends munit.CatsEffectSuite {
  test("switching view should be true during the effect and false after") {
    mkView.flatMap { (ref, view) =>
      for {
        _ <- ref.get.assertEquals(false)
        _ <- ref.get.assertEquals(true).switching(view)
        _ <- ref.get.assertEquals(false)
      } yield ()
    }
  }

  test("should still reset to false if the effect is cancelled") {
    mkView.flatMap { (ref, view) =>
      IO.never.switching(view).start.flatMap { fiber =>
        for {
          // Set to true so we can verify that it gets reset to false when cancelling
          _ <- view.set(true)
          _ <- ref.get.assertEquals(true)
          _ <- IO.cede *> fiber.cancel
          _ <- ref.get.assertEquals(false)
        } yield ()
      }
    }
  }

  test("should switch between two given values") {
    mkView.flatMap { (ref, view) =>
      for {
        _ <- ref.get.assertEquals(false)
        _ <- ref.get.assertEquals(true).switching(view, true, false)
        _ <- ref.get.assertEquals(false)
      } yield ()
    }
  }

  /**
   * Make a ViewF[IO, Boolean], backed by a `Ref`
   */
  private def mkView = Ref[IO]
    .of(false)
    .map(ref =>
      (ref,
       ViewF.apply[IO, Boolean](
         false,
         (f, cb) =>
           ref
             .modify: previous =>
               val current = f(previous)
               (current, (previous, current))
             .flatMap((previous, current) => cb(previous, current))
       )
      )
    )
}
