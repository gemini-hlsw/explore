// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import crystal.react.Ctx
import japgolly.scalajs.react.Reusability
import monocle.macros.Lenses

object AppCtx extends Ctx[IO, AppContextIO]

@Lenses
case class HelpContext(msg: Option[String])
object HelpContext {
  implicit val helpContextReuse: Reusability[HelpContext] = Reusability.derive
}

object HelpCtx extends Ctx[IO, HelpContext]
