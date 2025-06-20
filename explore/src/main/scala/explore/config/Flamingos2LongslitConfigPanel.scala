// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class Flamingos2LongslitConfigPanel(
  programId:                Program.Id,
  obsId:                    Observation.Id,
  calibrationRole:          Option[CalibrationRole],
  observingMode:            Aligner[ObservingMode.Flamingos2LongSlit, Flamingos2LongSlitInput],
  exposureTimeMode:         View[Option[ExposureTimeMode]],
  spectroscopyRequirements: View[ScienceRequirements.Spectroscopy],
  revertConfig:             Callback,
  confMatrix:               SpectroscopyModesMatrix,
  sequenceChanged:          Callback,
  readonly:                 Boolean,
  units:                    WavelengthUnits,
  isStaff:                  Boolean
) extends ReactFnProps(Flamingos2LongslitConfigPanel)

object Flamingos2LongslitConfigPanel
    extends ReactFnComponent[Flamingos2LongslitConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        modeData  <-
          useModeData(props.confMatrix, props.spectroscopyRequirements.get, props.observingMode.get)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val disableAdvancedEdit = editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
        val disableSimpleEdit   =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit

        val disperserView: View[Flamingos2Disperser] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.disperser,
            Flamingos2LongSlitInput.disperser.modify
          )
          .view(_.assign)

        val filterView: View[Flamingos2Filter] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.filter,
            Flamingos2LongSlitInput.filter.modify
          )
          .view(_.assign)

        val fpuView: View[Flamingos2Fpu] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.fpu,
            Flamingos2LongSlitInput.fpu.modify
          )
          .view(_.assign)

        val readModeView: View[Option[Flamingos2ReadMode]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitReadMode,
            Flamingos2LongSlitInput.explicitReadMode.modify
          )
          .view(_.orUnassign)

        given Enumerated[Option[Flamingos2ReadMode]] =
          deriveOptionalEnumerated[Flamingos2ReadMode]("Auto")
        given Display[Option[Flamingos2ReadMode]]    =
          deriveOptionalDisplay[Flamingos2ReadMode]("Auto")

        val deckerView: View[Option[Flamingos2Decker]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitDecker,
            Flamingos2LongSlitInput.explicitDecker.modify
          )
          .view(_.orUnassign)

        val defaultDecker = props.observingMode.get.defaultDecker

        <.div(
          ExploreStyles.AdvancedConfigurationGrid
        )(
          <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol1)(
            CustomizableEnumSelect(
              id = "disperser".refined,
              view = disperserView,
              defaultValue = props.observingMode.get.initialDisperser,
              label = "Disperser".some,
              helpId = Some("configuration/f2/disperser.md".refined),
              disabled = disableSimpleEdit
            ),
            CustomizableEnumSelect(
              id = "filter".refined,
              view = filterView,
              defaultValue = props.observingMode.get.initialFilter,
              label = "Filter".some,
              helpId = Some("configuration/f2/filter.md".refined),
              disabled = disableSimpleEdit
            ),
            CustomizableEnumSelect(
              id = "fpu".refined,
              view = fpuView,
              defaultValue = props.observingMode.get.initialFpu,
              label = "FPU".some,
              helpId = Some("configuration/f2/fpu.md".refined),
              disabled = disableSimpleEdit
            ),
            CustomizableEnumSelect(
              id = "read-mode".refined,
              view = readModeView,
              defaultValue = None,
              label = "Read Mode".some,
              helpId = Some("configuration/f2/read-mode.md".refined),
              disabled = disableSimpleEdit
            )
          ),
          <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol2)(
            ExposureTimeModeEditor(
              props.observingMode.get.instrument.some,
              props.spectroscopyRequirements.get.wavelength,
              props.exposureTimeMode,
              ScienceMode.Spectroscopy,
              props.readonly,
              props.units,
              props.calibrationRole
            )
          ),
          <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AdvancedConfigurationCol3)(
            FormLabel(htmlFor = "decker".refined)("Decker",
                                                  HelpIcon("configuration/f2/decker.md".refined)
            ),
            if (props.isStaff)
              CustomizableEnumSelectOptional(
                id = "decker".refined,
                view = deckerView.withDefault(defaultDecker),
                defaultValue = defaultDecker.some,
                disabled = disableAdvancedEdit
              )
            else
              <.label(^.id := "decker",
                      ExploreStyles.FormValue,
                      deckerView.get.getOrElse(defaultDecker).shortName
              ),
            // Per Andy, we'll use the wavelength of the filter as the central wavelength
            LambdaAndIntervalFormValues(
              modeData = modeData,
              centralWavelength = filterView.get.wavelength,
              units = props.units
            )
          ),
          AdvancedConfigButtons(
            editState = editState,
            isCustomized = props.observingMode.get.isCustomized,
            revertConfig = props.revertConfig,
            revertCustomizations = props.observingMode.view(_.toInput).mod(_.revertCustomizations),
            sequenceChanged = props.sequenceChanged,
            readonly = props.readonly,
            showAdvancedButton = props.isStaff
          )
        )
    )
