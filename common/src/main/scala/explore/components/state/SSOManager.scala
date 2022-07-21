// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import explore.model.UserVault
import japgolly.scalajs.react._
import org.typelevel.log4cats.Logger
import react.common._

import java.time.Instant

final case class SSOManager(
  expiration:       Instant,
  setVault:         Option[UserVault] => Callback,
  setMessage:       NonEmptyString => Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[SSOManager](SSOManager.component)

object SSOManager {
  protected type Props = SSOManager

  private def tokenRefresher(
    expiration:   Instant,
    setVault:     Option[UserVault] => Callback,
    setMessage:   NonEmptyString => Callback
  )(implicit ctx: AppContextIO): IO[Unit] =
    for {
      vaultOpt <- ctx.sso.refreshToken(expiration)
      _        <- setVault(vaultOpt).to[IO]
      _        <- vaultOpt.fold(setMessage("Your session has expired").to[IO])(vault =>
                    tokenRefresher(vault.expiration, setVault, setMessage)
                  )
    } yield ()

  protected val component = ScalaFnComponent
    .withHooks[Props]
    .useRef(none[IO[Unit]])             // cancelToken
    .useAsyncEffectOnMountBy { (props, cancelToken) =>
      implicit val ctx = props.ctx

      tokenRefresher(props.expiration, props.setVault, props.setMessage)
        .onError(t =>
          Logger[IO].error(t)("Error refreshing SSO token") >>
            (props.setVault(none) >>
              props.setMessage("There was an error while checking the validity of your session"))
              .to[IO]
        )
        .start
        .flatMap(fiber => cancelToken.setAsync(fiber.cancel.some))
        .as(
          cancelToken.getAsync >>= (cancelOpt =>
            cancelOpt.foldMap(_ >> props.setVault(none).to[IO])
          )
        )
    }
    .render((_, _) => React.Fragment()) // This is a "phantom" component. Doesn't render anything.
}
