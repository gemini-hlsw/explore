// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptySet
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.attachments.AttachmentsTile
import explore.common.Aligner
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.ExploreGridLayouts
import explore.model.Group
import explore.model.GroupList
import explore.model.ObsAttachmentAssignmentMap
import explore.model.ObservationList
import explore.model.OverviewTabTileIds
import explore.model.ProgramDetails
import explore.model.TargetAttachmentAssignmentMap
import explore.model.enums.GridLayoutSection
import explore.model.enums.GroupWarning
import explore.model.layout.LayoutsMap
import explore.undo.*
import explore.validations.GroupWarningsTile
import explore.validations.ObservationValidationsTableBody
import explore.validations.ObservationValidationsTableTileState
import explore.validations.ObservationValidationsTableTitle
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramType
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.OptionNonEmptyStringIso
import lucuma.ui.primereact.FormInputTextAreaView
import lucuma.ui.primereact.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given

case class OverviewTabContents(
  programId:                   Program.Id,
  userVault:                   Option[UserVault],
  undoer:                      Undoer,
  attachments:                 View[AttachmentList],
  obsAttachmentAssignments:    ObsAttachmentAssignmentMap,
  targetAttachmentAssignments: TargetAttachmentAssignmentMap,
  observations:                View[ObservationList],
  groups:                      GroupList,
  groupWarnings:               Map[Group.Id, NonEmptySet[GroupWarning]],
  detailsUndoSetter:           UndoSetter[ProgramDetails],
  layout:                      LayoutsMap,
  proposalIsAccepted:          Boolean,
  readonly:                    Boolean
) extends ReactFnProps(OverviewTabContents):
  val userId: Option[User.Id] = userVault.map(_.user.id)

object OverviewTabContents
    extends ReactFnComponent[OverviewTabContents](props =>
      for {
        ctx    <- useContext(AppContext.ctx)
        resize <- useResizeDetector
      } yield
        import ctx.given

        val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.OverviewLayout)

        val warningsAndErrorsTile = Tile(
          OverviewTabTileIds.WarningsAndErrorsId.id,
          "Warnings And Errors",
          ObservationValidationsTableTileState(_ => Callback.empty)
        )(
          ObservationValidationsTableBody(props.userId, props.programId, props.observations, _),
          ObservationValidationsTableTitle.apply
        )

        val groupWarningsTile =
          GroupWarningsTile.apply(props.userId, props.programId, props.groups, props.groupWarnings)

        val showObsAttachments =
          props.proposalIsAccepted || props.detailsUndoSetter.get.programType =!= ProgramType.Science

        val attachmentsTile =
          AttachmentsTile(
            props.programId,
            props.userVault,
            props.obsAttachmentAssignments,
            props.targetAttachmentAssignments,
            props.attachments,
            showObsAttachments,
            props.readonly
          )

        // only edit program description here for non-science programs. For science programs it
        // is edited as the abstract on the proposals tab.
        val descriptionTile =
          if (props.detailsUndoSetter.get.programType === ProgramType.Science)
            Tile.dummyTile(OverviewTabTileIds.DescriptionId.id)
          else
            val descriptionAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
              Aligner(
                props.detailsUndoSetter,
                UpdateProgramsInput(
                  WHERE = props.programId.toWhereProgram.assign,
                  SET = ProgramPropertiesInput()
                ),
                odbApi.updateProgram(_)
              ).zoom(ProgramDetails.description,
                     UpdateProgramsInput.SET.andThen(ProgramPropertiesInput.description).modify
              )
            val descriptionView                                                            =
              descriptionAligner.view(_.orUnassign)

            Tile(OverviewTabTileIds.DescriptionId.id,
                 "Description",
                 bodyClass = ExploreStyles.ProgramDescription
            )(
              _ =>
                FormInputTextAreaView(id = "program-description".refined,
                                      descriptionView.as(OptionNonEmptyStringIso)
                ),
              (_, _) =>
                <.div(ExploreStyles.TitleUndoButtons)(
                  UndoButtons(props.undoer, disabled = props.readonly)
                )
            )

        <.div(ExploreStyles.MultiPanelTile)(
          TileController(
            props.userId,
            resize.width.getOrElse(1),
            defaultLayouts,
            props.layout,
            List(
              warningsAndErrorsTile,
              groupWarningsTile,
              attachmentsTile,
              descriptionTile
            ),
            GridLayoutSection.OverviewLayout
          )
        ).withRef(resize.ref)
    )
