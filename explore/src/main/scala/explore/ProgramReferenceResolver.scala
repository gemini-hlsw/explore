// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.option.*
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import explore.model.AppContext
import explore.model.Focused
import explore.model.enums.AppTab
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ProgramReference
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message.Severity
import lucuma.ui.syntax.pot.*
import queries.common.ProgramQueriesGQL.ResolveProgramReference

case class ProgramReferenceResolver(programRef: ProgramReference)
    extends ReactFnProps(ProgramReferenceResolver.component)

object ProgramReferenceResolver:
  private type Props = ProgramReferenceResolver

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useEffectResultOnMountBy: (props, ctx) =>
        import ctx.given

        ResolveProgramReference[IO]
          .query(props.programRef.assign)
          .raiseGraphQLErrors
          .flatMap: data =>
            data.program
              .map: p =>
                ctx.pushPage((AppTab.Overview, p.id, Focused.None).some).to[IO]
              .getOrElse:
                ToastCtx[IO].showToast(
                  s"Program reference ${props.programRef.label} does not exist.",
                  Severity.Error
                ) >> ctx.pushPage(none).to[IO]
      .render: (props, _, result) =>
        result.renderPot(_ => EmptyVdom)
