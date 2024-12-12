// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import explore.model.AppContext
import explore.model.Focused
import explore.model.enums.AppTab
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ObservationReference
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.pot.*
import queries.common.ObsQueriesGQL.ResolveObsReference

case class ObsReferenceResolver(obsRef: ObservationReference)
    extends ReactFnProps(ObsReferenceResolver.component)

object ObsReferenceResolver:
  private type Props = ObsReferenceResolver

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useEffectResultOnMountBy: (props, ctx) =>
        import ctx.given

        ResolveObsReference[IO]
          .query(props.obsRef.assign)
          .flatMap: data =>
            data.observation
              .map: o =>
                ctx.pushPage(AppTab.Observations, o.program.id, Focused.singleObs(o.id)).to[IO]
              .getOrElse:
                IO.raiseError:
                  RuntimeException(s"Observation reference ${props.obsRef.label} does not exist")
      .render: (props, _, result) =>
        result.renderPot(_ => EmptyVdom)
