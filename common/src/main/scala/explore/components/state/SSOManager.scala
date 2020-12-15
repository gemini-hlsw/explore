// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import java.time.Instant

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.implicits._
import explore.model.UserVault
import explore.model.reusability._
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.common.ReactProps

final case class SSOManager(
  expiration: Instant,
  setVault:   Option[UserVault] => IO[Unit],
  setMessage: NonEmptyString => IO[Unit]
) extends ReactProps[SSOManager](SSOManager.component)

object SSOManager {
  type Props = SSOManager

  @Lenses
  case class State(cancelToken: Option[IO[Unit]])

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.expiration)
  implicit val stateReuse: Reusability[State] = Reusability.never

  final class Backend() {

    def tokenRefresher(
      expiration: Instant,
      setVault:   Option[UserVault] => IO[Unit],
      setMessage: NonEmptyString => IO[Unit]
    ): IO[Unit] =
      AppCtx.flatMap(implicit ctx =>
        for {
          vaultOpt <- ctx.sso.refreshToken(expiration)
          _        <- setVault(vaultOpt)
          _        <- vaultOpt.fold(setMessage("Your session has expired"))(vault =>
                        tokenRefresher(vault.expiration, setVault, setMessage)
                      )
        } yield ()
      )

    // This is a "phantom" component. Doesn't render anything.
    def render(): VdomNode = React.Fragment()
  }

  val component = ScalaComponent
    .builder[Props]
    .initialState(State(none))
    .renderBackend[Backend]
    .componentDidMount { $ =>
      $.backend
        .tokenRefresher($.props.expiration, $.props.setVault, $.props.setMessage)
        .runCancelable {
          case Left(t) =>
            AppCtx.flatMap(implicit ctx => Logger[IO].error(t)("Error refreshing SSO token")) >>
              $.props.setVault(none) >>
              $.props.setMessage("There was an error while checking the validity of your session")
          case _       => IO.unit
        }
        .toIO
        .flatMap(ct => $.modStateIn[IO](State.cancelToken.set(ct.some)))
        .runAsyncCB
    }
    .componentWillUnmount($ =>
      // Setting vault to none is defensive. This component should actually unmount when vault is none.
      $.state.cancelToken.map(cancel => (cancel >> $.props.setVault(none))).orEmpty.runAsyncCB
    )
    .shouldComponentUpdateConst(false)
    .build
}
