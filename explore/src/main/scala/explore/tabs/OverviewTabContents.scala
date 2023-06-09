// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.UnderConstruction
import explore.attachments.ObsAttachmentsTable
import explore.components.Tile
import explore.components.TileController
import explore.model.AppContext
import explore.model.ObsAttachment
import explore.model.ObsAttachmentAssignmentMap
import explore.model.ObsAttachmentList
import explore.model.UserVault
import explore.model.enums.GridLayoutSection
import explore.model.layout.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.refined.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.gridlayout.*
import react.primereact.Button
import react.resizeDetector.hooks.*

case class OverviewTabContents(
  programId:                Program.Id,
  userVault:                Option[UserVault],
  obsAttachments:           View[ObsAttachmentList],
  obsAttachmentAssignments: ObsAttachmentAssignmentMap
) extends ReactFnProps(OverviewTabContents.component)

object OverviewTabContents {
  private type Props = OverviewTabContents

  private val WarningsAndErrorsHeight: NonNegInt    = 8.refined
  private val WarningsAndErrorsMinHeight: NonNegInt = 6.refined
  private val ObsAttachmentsHeight: NonNegInt       = 8.refined
  private val ObsAttachmentsMinHeight: NonNegInt    = 6.refined
  private val TileMinWidth: NonNegInt               = 4.refined
  private val DefaultWidth: NonNegInt               = 10.refined
  private val DefaultLargeWidth: NonNegInt          = 12.refined

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(
        i = ObsTabTilesIds.WarningsAndErrorsId.id.value,
        x = 0,
        y = 0,
        w = DefaultWidth.value,
        h = WarningsAndErrorsHeight.value,
        minH = WarningsAndErrorsMinHeight.value,
        minW = TileMinWidth.value
      ),
      LayoutItem(
        i = ObsTabTilesIds.ObsAttachmentsId.id.value,
        x = 0,
        y = WarningsAndErrorsHeight.value,
        w = DefaultWidth.value,
        h = ObsAttachmentsHeight.value,
        minH = ObsAttachmentsMinHeight.value,
        minW = TileMinWidth.value
      )
    )
  )

  private val defaultLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg,
       layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
      ),
      (BreakpointName.md, layoutMedium)
    )
  )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // TODO: Save/restore the layout in user prefs.
      .useStateView(Pot(defaultLayouts))
      .useResizeDetector()
      .render { (props, ctx, layouts, resize) =>

        import ctx.given

        layouts.renderPotView { l =>

          val warningsAndErrorsTile = Tile(
            ObsTabTilesIds.WarningsAndErrorsId.id,
            "Warnings And Errors",
            none,
            canMinimize = true
          )(_ => UnderConstruction())

          val obsAttachmentsTile = Tile(
            ObsTabTilesIds.ObsAttachmentsId.id,
            "Observation Attachments",
            none,
            canMinimize = true
          )(renderInTitle =>
            Pot
              .fromOption(props.userVault)
              .renderPot(vault =>
                ObsAttachmentsTable(props.programId,
                                    vault.token,
                                    props.obsAttachments,
                                    props.obsAttachmentAssignments,
                                    renderInTitle
                )
              )
          )

          TileController(
            props.userVault.map(_.user.id),
            resize.width.getOrElse(1),
            defaultLayouts,
            l,
            List(warningsAndErrorsTile, obsAttachmentsTile),
            GridLayoutSection.OverviewLayout,
            storeLayout = false
          )
        }
      }
}
