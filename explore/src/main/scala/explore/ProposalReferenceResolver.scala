// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.option.*
import crystal.react.*
import crystal.react.hooks.*
import explore.model.AppContext
import explore.model.Focused
import explore.model.enums.AppTab
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ProposalReference
import lucuma.react.common.ReactFnComponent
import lucuma.refined.refined
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message.Severity
import lucuma.ui.syntax.pot.*

case class ProposalReferenceResolver(proposalRef: ProposalReference)
    extends ReactFnProps(ProposalReferenceResolver)

object ProposalReferenceResolver
    extends ReactFnComponent[ProposalReferenceResolver](props =>
      for
        ctx    <- useContext(AppContext.ctx)
        result <- useEffectResultOnMount:
                    import ctx.given
                    ctx.odbApi
                      .resolveProposalReference(props.proposalRef)
                      .flatMap:
                        _.map: programId =>
                          ctx.pushPage((AppTab.Proposal, programId, Focused.None).some).to[IO]
                        .getOrElse:
                          ToastCtx[IO].showToast(
                            s"Proposal reference ${props.proposalRef.label} does not exist",
                            Severity.Error
                          ) >> ctx.pushPage(none).to[IO]
      yield result.value.renderPot("proposalRefResolver".refined, _ => EmptyVdom)
    )
