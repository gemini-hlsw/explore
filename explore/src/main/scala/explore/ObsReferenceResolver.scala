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
import lucuma.core.model.ObservationReference
import lucuma.react.common.*
import lucuma.react.primereact.Message.Severity
import lucuma.ui.syntax.pot.*
import queries.common.ObsQueriesGQL.ResolveObsReference

case class ObsReferenceResolver(obsRef: ObservationReference)
    extends ReactFnProps(ObsReferenceResolver)

object ObsReferenceResolver
    extends ReactFnComponent[ObsReferenceResolver](props =>
      for
        ctx    <- useContext(AppContext.ctx)
        result <- useEffectResultOnMount:
                    import ctx.given

                    ResolveObsReference[IO]
                      .query(props.obsRef.assign)
                      .raiseGraphQLErrors
                      .flatMap: data =>
                        data.observation
                          .map: o =>
                            ctx
                              .pushPage:
                                (AppTab.Observations, o.program.id, Focused.singleObs(o.id)).some
                              .to[IO]
                          .getOrElse:
                            ToastCtx[IO].showToast(
                              s"Observation reference ${props.obsRef.label} does not exist.",
                              Severity.Error
                            ) >> ctx.pushPage(none).to[IO]
      yield result.value.renderPot(_ => EmptyVdom)
    )
