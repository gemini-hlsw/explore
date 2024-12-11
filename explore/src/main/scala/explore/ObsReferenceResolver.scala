// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.core.model.ObservationReference
import explore.model.AppContext
import cats.effect.IO
import crystal.react.hooks.*
import queries.common.ObsQueriesGQL.ResolveObsReference
import clue.data.syntax.*
import explore.model.enums.AppTab
import explore.model.Focused
import crystal.react.*
import lucuma.ui.syntax.pot.*

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
