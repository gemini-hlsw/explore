// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import coulomb.*
import coulomb.units.si.Kelvin
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string
import explore.*
import explore.common.*
import explore.components.FileUploadButton
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.AppContext
import explore.model.display.given
import explore.model.enums.SedType
import explore.model.enums.SedTypeEnum
import explore.model.syntax.all.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.FluxDensityContinuumValueRefinement
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.model.Attachment
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.SelectItem
import lucuma.react.primereact.TooltipOptions
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedMap

private abstract class SpectralDefinitionEditorBuilder[
  T,
  S,
  Props <: SpectralDefinitionEditor[T, S]
](sedTypeEnum: SedTypeEnum[T])(using enumFDCUnits: Enumerated[Units Of FluxDensityContinuum[T]]) {
  import sedTypeEnum.given

  protected def brightnessEditor
    : (View[SortedMap[Band, BrightnessMeasure[T]]], View[IsExpanded], Boolean) => VdomNode
  protected def emissionLineEditor
    : (View[SortedMap[Wavelength, EmissionLine[T]]], View[IsExpanded], Boolean) => VdomNode

  protected def currentType: SpectralDefinition[T] => Option[SedType[T]]
  protected def userDefinedType: SedType[T]

  protected[spectralDefinition] val component = ScalaFnComponent[Props]: props =>
    for
      ctx                   <- useContext(AppContext.ctx)
      given Logger[IO]       = ctx.logger
      currentSedType         = currentType(props.spectralDefinition.get)
      sedType               <- useStateView(currentSedType)
      customSedAttachmentId <- useStateView(props.currentCustomSedAttachmentId)
      _                     <- // keep state updated.
        useEffectWithDeps(currentSedType, props.currentCustomSedAttachmentId):
          (newSedType, newAttachmentId) =>
            sedType.set(newSedType).when_(newSedType =!= sedType.get) >>
              customSedAttachmentId
                .set(newAttachmentId)
                .when_(newAttachmentId =!= customSedAttachmentId.get)
      _                     <- // Update model when state is fully defined.
        useEffectWithDeps(sedType.get, customSedAttachmentId.get):
          case (Some(someSedType), Some(attachmentId))
              if someSedType === userDefinedType &&
                (!currentSedType.contains_(someSedType) ||
                  !props.currentCustomSedAttachmentId.contains_(attachmentId)) =>
            props.modSpectralDefinition:
              sedTypeEnum.toBandNormalized(UnnormalizedSED.UserDefinedAttachment(attachmentId))
          case (Some(sed @ SedType.Immediate(_, convert)), _) if !currentSedType.contains_(sed) =>
            props.modSpectralDefinition(convert)
          case (None, _) if currentSedType.isDefined                                            =>
            props.modSpectralDefinition(SpectralDefinition.unnormalizedSED.replace(none))
          case _                                                                                =>
            Callback.empty
    yield
      val isCustomSed: Boolean =
        sedType.get.contains_(userDefinedType)

      val isFullyDefined: Boolean =
        sedType.get.isDefined && (!isCustomSed || customSedAttachmentId.get.isDefined)

      val stellarLibrarySpectrumAlignerOpt
        : Option[Aligner[StellarLibrarySpectrum, Input[StellarLibrarySpectrum]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.stellarLibrary
              .andThen:
                UnnormalizedSED.StellarLibrary.librarySpectrum
            ,
            UnnormalizedSedInput.stellarLibrary.modify
          )

      val coolStarTemperatureAlignerOpt
        : Option[Aligner[CoolStarTemperature, Input[CoolStarTemperature]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.coolStarModel.andThen(UnnormalizedSED.CoolStarModel.temperature),
            UnnormalizedSedInput.coolStar.modify
          )

      val galaxySpectrumAlignerOpt: Option[Aligner[GalaxySpectrum, Input[GalaxySpectrum]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.galaxy.andThen(UnnormalizedSED.Galaxy.galaxySpectrum),
            UnnormalizedSedInput.galaxy.modify
          )

      val planetSpectrumAlignerOpt: Option[Aligner[PlanetSpectrum, Input[PlanetSpectrum]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.planet.andThen(UnnormalizedSED.Planet.planetSpectrum),
            UnnormalizedSedInput.planet.modify
          )

      val quasarSpectrumAlignerOpt: Option[Aligner[QuasarSpectrum, Input[QuasarSpectrum]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.quasar.andThen(UnnormalizedSED.Quasar.quasarSpectrum),
            UnnormalizedSedInput.quasar.modify
          )

      val hiiRegionSpectrumAlignerOpt
        : Option[Aligner[HIIRegionSpectrum, Input[HIIRegionSpectrum]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.hiiRegion.andThen(UnnormalizedSED.HIIRegion.hiiRegionSpectrum),
            UnnormalizedSedInput.hiiRegion.modify
          )

      val planetaryNebulaSpectrumAlignerOpt
        : Option[Aligner[PlanetaryNebulaSpectrum, Input[PlanetaryNebulaSpectrum]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.planetaryNebula.andThen:
              UnnormalizedSED.PlanetaryNebula.planetaryNebulaSpectrum
            ,
            UnnormalizedSedInput.planetaryNebula.modify
          )

      val powerLawIndexAlignerOpt: Option[Aligner[BigDecimal, Input[BigDecimal]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.powerLaw.andThen(UnnormalizedSED.PowerLaw.index),
            UnnormalizedSedInput.powerLaw.modify
          )

      val blackBodyTemperatureAlignerOpt: Option[Aligner[Quantity[PosInt, Kelvin], Input[PosInt]]] =
        props.sedAlignerOpt.flatMap:
          _.zoomOpt(
            UnnormalizedSED.blackBody.andThen(UnnormalizedSED.BlackBody.temperature),
            UnnormalizedSedInput.blackBodyTempK.modify
          )

      def spectrumRow[T: Enumerated: Display](id: string.NonEmptyString, view: View[T]) =
        EnumDropdownView(
          id = id,
          value = view,
          clazz = LucumaPrimeStyles.FormField,
          disabled = props.disabled
        )

      React
        .Fragment(
          props.catalogInfo.flatMap(ci =>
            ci.objectType.map(ot =>
              FormInputText(
                id = "catalogInfo".refined,
                value = ot,
                label = React.Fragment(
                  ci.catalog match
                    case CatalogName.Import => "Object Type"
                    case other              => other.shortName
                  ,
                  HelpIcon("target/main/target-catalog-info.md".refined)
                ),
                disabled = true
              )
            )
          ),
          FormLabel(htmlFor = "sed".refined)("SED", HelpIcon("target/main/target-sed.md".refined)),
          <.div(
            ExploreStyles.SEDTypeDropdown |+| ExploreStyles.WarningInput
              .when_(sedType.get.isEmpty && props.calibrationRole.needsITC),
            EnumDropdownOptionalView(
              id = "sed".refined,
              value = sedType,
              clazz = LucumaPrimeStyles.FormField,
              disabled = props.disabled
            ),
            <.span(PrimeStyles.InputGroupAddon, ^.borderRight := 0.px)(
              props.calibrationRole.renderRequiredForITCIcon
            ).when(sedType.get.isEmpty)
          ),
          if (isFullyDefined)
            React.Fragment(
              stellarLibrarySpectrumAlignerOpt
                .map(rsu => spectrumRow("slSpectrum".refined, rsu.view(_.assign))),
              coolStarTemperatureAlignerOpt
                .map(rsu => spectrumRow("csTemp".refined, rsu.view(_.assign))),
              galaxySpectrumAlignerOpt
                .map(rsu => spectrumRow("gSpectrum".refined, rsu.view(_.assign))),
              planetSpectrumAlignerOpt
                .map(rsu => spectrumRow("pSpectrum".refined, rsu.view(_.assign))),
              quasarSpectrumAlignerOpt
                .map(rsu => spectrumRow("qSpectrum".refined, rsu.view(_.assign))),
              hiiRegionSpectrumAlignerOpt
                .map(rsu => spectrumRow("hiirSpectrum".refined, rsu.view(_.assign))),
              planetaryNebulaSpectrumAlignerOpt
                .map(rsu => spectrumRow("pnSpectrum".refined, rsu.view(_.assign))),
              powerLawIndexAlignerOpt
                .map(rsu =>
                  FormInputTextView( // Power-law index can be any decimal
                    id = "powerLawIndex".refined,
                    value = rsu.view(_.assign),
                    label = "Index",
                    validFormat = InputValidSplitEpi.bigDecimal,
                    changeAuditor =
                      ChangeAuditor.fromInputValidSplitEpi(InputValidSplitEpi.bigDecimal),
                    disabled = props.disabled
                  )
                ),
              blackBodyTemperatureAlignerOpt
                .map(rsu =>
                  FormInputTextView( // Temperature is in K, a positive integer
                    id = "bbTempK".refined,
                    value = rsu.view(_.value.assign).stripQuantity,
                    label = "Temperature",
                    validFormat = InputValidSplitEpi.posInt,
                    changeAuditor = ChangeAuditor
                      .fromInputValidSplitEpi(InputValidSplitEpi.posInt)
                      .denyNeg,
                    units = "°K",
                    disabled = props.disabled
                  )
                )
            )
          else EmptyVdom,
          React.Fragment(
            if (isCustomSed)
              React.Fragment(
                <.span(LucumaPrimeStyles.FormField, ExploreStyles.SEDTypeDropdown)(
                  DropdownOptional(
                    id = "customSed",
                    value = customSedAttachmentId.get,
                    options = props.customSedAttachments.map: a =>
                      SelectItem(value = a.id, label = a.fileName),
                    onChange = v => customSedAttachmentId.set(v),
                    placeholder =
                      if (props.customSedAttachments.nonEmpty) "Select custom SED file"
                      else "No custom SED files",
                    disabled = props.disabled
                  ),
                  FileUploadButton(
                    props.programId,
                    props.attachments,
                    AttachmentType.CustomSED,
                    aid => // Won't work without delay
                      customSedAttachmentId
                        .set(aid.some)
                        .delayMs(1)
                        .toCallback,
                    props.disabled,
                    props.authToken
                  )
                )
              )
            else EmptyVdom,
            if (isFullyDefined)
              React.Fragment(
                props.bandBrightnessesViewOpt
                  .map(bandBrightnessesView =>
                    <.div(ExploreStyles.BrightnessesTableWrapper)(
                      brightnessEditor(bandBrightnessesView,
                                       props.brightnessExpanded,
                                       props.disabled
                      )
                    )
                  ),
                props.fluxDensityContinuumOpt
                  .map(fluxDensityContinuum =>
                    React.Fragment(
                      FormLabel(htmlFor = "fluxValue".refined)("Continuum"),
                      <.div(
                        ExploreStyles.FlexContainer |+| LucumaPrimeStyles.FormField,
                        FormInputTextView(
                          id = "fluxValue".refined,
                          value = fluxDensityContinuum.zoom(Measure.valueTagged),
                          validFormat = InputValidSplitEpi
                            .refinedBigDecimalWithScientificNotation[
                              FluxDensityContinuumValueRefinement
                            ]
                            .andThen(FluxDensityContinuumValue.Value.reverse),
                          changeAuditor = ChangeAuditor.posScientificNotation(),
                          disabled = props.disabled
                        ),
                        EnumDropdownView(
                          id = "Units".refined,
                          value = fluxDensityContinuum.zoom(Measure.unitsTagged),
                          disabled = props.disabled
                        )
                      )
                    )
                  ),
                props.emissionLinesViewOpt
                  .map(e =>
                    <.div(ExploreStyles.BrightnessesTableWrapper)(
                      emissionLineEditor(e, props.brightnessExpanded, props.disabled)
                    )
                  )
              )
            else EmptyVdom
          )
        )
}
