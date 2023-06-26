// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.hooks.*
import crystal.react.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.AppContext
import explore.model.UserVault
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import react.common.ReactFnProps

import java.time.Instant

case class SSOManager(
  expiration: Instant,
  setVault:   Option[UserVault] => Callback,
  setMessage: NonEmptyString => Callback
) extends ReactFnProps(SSOManager.component)

object SSOManager:
  private type Props = SSOManager

  private def tokenRefresher(
    expiration: Instant,
    setVault:   Option[UserVault] => Callback,
    setMessage: NonEmptyString => Callback,
    ctx:        AppContext[IO]
  ): IO[Unit] =
    for {
      vaultOpt <- ctx.sso.refreshToken(expiration)
      _        <- setVault(vaultOpt).toAsync
      _        <- vaultOpt.fold(setMessage("Your session has expired".refined).toAsync)(vault =>
                    tokenRefresher(vault.expiration, setVault, setMessage, ctx)
                  )
    } yield ()

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useRef(none[IO[Unit]])         // cancelToken
    .useAsyncEffectOnMountBy { (props, ctx, cancelToken) =>
      import ctx.given

      tokenRefresher(props.expiration, props.setVault, props.setMessage, ctx)
        .onError(t =>
          Logger[IO].error(t)("Error refreshing SSO token") >>
            (props.setVault(none) >>
              props.setMessage(
                "There was an error while checking the validity of your session".refined
              )).toAsync
        )
        .start
        .flatMap(fiber => cancelToken.setAsync(fiber.cancel.some))
        .as(
          cancelToken.getAsync >>= (cancelOpt =>
            cancelOpt.foldMap(_ >> props.setVault(none).toAsync)
          )
        )
    }
    .render((_, _, _) => EmptyVdom) // This is a "phantom" component. Doesn't render anything.
