// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import clue.data.syntax.*
import crystal.react.hooks.*
import explore.common.*
import explore.components.HelpIcon
import explore.model.AppContext
import explore.model.enums.SourceProfileType
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.CatalogInfo
import lucuma.core.model.SourceProfile
import lucuma.core.model.SourceProfile.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.EnumDropdown
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import spectralDefinition.{IntegratedSpectralDefinitionEditor, SurfaceSpectralDefinitionEditor}

case class SourceProfileEditor(
  sourceProfile:   Aligner[SourceProfile, SourceProfileInput],
  catalogInfo:     Option[CatalogInfo],
  disabled:        Boolean,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps(SourceProfileEditor.component)

object SourceProfileEditor:
  private type Props = SourceProfileEditor

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(IsExpanded(true))
      .render { (props, ctx, brightnessExpanded) =>
        import ctx.given

        val gaussianAlignerOpt: Option[Aligner[Gaussian, GaussianInput]] =
          props.sourceProfile.zoomOpt(
            SourceProfile.gaussian,
            forceAssign(SourceProfileInput.gaussian.modify)(GaussianInput())
          )

        React.Fragment(
          FormLabel(htmlFor = "profile-type".refined)(
            "Profile",
            HelpIcon("target/main/target-profile.md".refined)
          ),
          EnumDropdown[SourceProfileType](
            id = "profile-type".refined,
            value = SourceProfileType.fromSourceProfile(props.sourceProfile.get),
            onChange = sp => props.sourceProfile.view(_.toInput).mod(sp.convert),
            clazz = LucumaPrimeStyles.FormField,
            disabled = props.disabled
          ),
          props.sourceProfile
            .zoomOpt(
              SourceProfile.point.andThen(Point.spectralDefinition),
              forceAssign(SourceProfileInput.point.modify)(SpectralDefinitionIntegratedInput())
            )
            .map(pointSpectralDefinitionAccess =>
              IntegratedSpectralDefinitionEditor(
                pointSpectralDefinitionAccess,
                props.catalogInfo,
                brightnessExpanded,
                props.disabled,
                props.calibrationRole
              )
            ),
          props.sourceProfile
            .zoomOpt(
              SourceProfile.uniform.andThen(Uniform.spectralDefinition),
              forceAssign(SourceProfileInput.uniform.modify)(SpectralDefinitionSurfaceInput())
            )
            .map(uniformSpectralDefinitionAccess =>
              SurfaceSpectralDefinitionEditor(
                uniformSpectralDefinitionAccess,
                props.catalogInfo,
                brightnessExpanded,
                props.disabled,
                props.calibrationRole
              )
            ),
          gaussianAlignerOpt
            .map(gaussianAligner =>
              React.Fragment(
                FormInputTextView(                          // FWHM is positive arcsec accepting decimals
                  id = "fwhm".refined,
                  value = gaussianAligner
                    .zoom(Gaussian.fwhm, GaussianInput.fwhm.modify)
                    .view(_.toInput.assign),
                  label = "FWHM",
                  validFormat = MathValidators.angleArcSec, // THIS IS ARCSEC AND NOT SIGNED!
                  changeAuditor = ChangeAuditor
                    .fromInputValidSplitEpi(MathValidators.angleArcSec)
                    .denyNeg
                    .allowEmpty,
                  units = "arcsec",
                  disabled = props.disabled
                ),
                IntegratedSpectralDefinitionEditor(
                  gaussianAligner.zoom(
                    Gaussian.spectralDefinition,
                    forceAssign(GaussianInput.spectralDefinition.modify)(
                      SpectralDefinitionIntegratedInput()
                    )
                  ),
                  props.catalogInfo,
                  brightnessExpanded,
                  props.disabled,
                  props.calibrationRole
                )
              )
            )
        )
      }
