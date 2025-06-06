// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import explore.Icons
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.ProposalTabTileIds
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps

object ProposalErrorsTile:
  def apply(errors: List[String]): Tile[Unit] =
    Tile(
      ProposalTabTileIds.ErrorsId.id,
      s"Errors (${errors.size})"
    )(_ => Body(errors))

  private case class Body(errors: List[String]) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>
        <.div(
          ExploreStyles.ProposalErrorsTile,
          props.errors.toTagMod(
            using
            e =>
              <.div(
                Icons.ErrorIcon,
                e
              )
          )
        )
      )
