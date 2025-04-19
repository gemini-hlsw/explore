// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.AsterismPreferences
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.components.ui.ExploreStyles
import explore.model.*
import explore.model.enums.Visible
import explore.optics.ModelOptics
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.common.*
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenu
import lucuma.react.primereact.PopupMenuRef
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Lens

case class AladinPreferencesMenu(
  uid:               User.Id,
  tids:              NonEmptyList[Target.Id],
  globalPreferences: View[GlobalPreferences],
  targetPreferences: View[AsterismVisualOptions],
  menuRef:           PopupMenuRef
) extends ReactFnProps(AladinPreferencesMenu.component)

object AladinPreferencesMenu extends ModelOptics with AladinCommon:

  private type Props = AladinPreferencesMenu

  private val unsafeRangeLens: Lens[AsterismVisualOptions.ImageFilterRange, Double] =
    Lens[AsterismVisualOptions.ImageFilterRange, Double](_.value.toDouble)(x =>
      y =>
        refineV[AsterismVisualOptions.FilterRange](x.toInt).toOption
          .getOrElse(y) // Ignore invalid updates
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Reference to root
      .useMemo(())(_ => domRoot)
      .render {
        (
          props,
          ctx,
          root
        ) =>
          import ctx.given

          def prefsSetter(
            saturation: Option[Int] = None,
            brightness: Option[Int] = None
          ): Callback =
            AsterismPreferences
              .updateAladinPreferences[IO](
                props.targetPreferences.get.id,
                props.uid,
                props.tids,
                saturation = saturation,
                brightness = brightness
              )
              .flatMap(id => props.targetPreferences.zoom(AsterismVisualOptions.id).set(id).to[IO])
              .runAsync
              .void

          def visiblePropView(
            get:   Lens[GlobalPreferences, Visible],
            onMod: Visible => Callback
          ) =
            props.globalPreferences
              .zoom(get)
              .withOnMod(onMod)
              .as(Visible.Value)

          val agsCandidatesView =
            visiblePropView(
              GlobalPreferences.showCatalog,
              v => userPrefsSetter(props.uid, showCatalog = v.some)
            )

          val agsOverlayView =
            visiblePropView(
              GlobalPreferences.agsOverlay,
              v => userPrefsSetter(props.uid, agsOverlay = v.some)
            )

          val scienceOffsetsView =
            visiblePropView(
              GlobalPreferences.scienceOffsets,
              v => userPrefsSetter(props.uid, scienceOffsets = v.some)
            )

          val acquisitionOffsetsView =
            visiblePropView(
              GlobalPreferences.acquisitionOffsets,
              v => userPrefsSetter(props.uid, acquisitionOffsets = v.some)
            )

          def cssVarView(
            varLens:        Lens[AsterismVisualOptions, AsterismVisualOptions.ImageFilterRange],
            variableName:   String,
            updateCallback: Int => Callback
          ) =
            props.targetPreferences
              .zoom(varLens)
              .withOnMod(s => setVariable(root, variableName, s) *> updateCallback(s))

          val saturationView =
            cssVarView(AsterismVisualOptions.saturation,
                       "saturation",
                       s => prefsSetter(saturation = s.some)
            )
          val brightnessView =
            cssVarView(AsterismVisualOptions.brightness,
                       "brightness",
                       s => prefsSetter(brightness = s.some)
            )

          val allowMouseZoomView =
            props.globalPreferences
              .zoom(GlobalPreferences.aladinMouseScroll)
              .withOnMod(z =>
                GlobalUserPreferences.storeAladinPreferences[IO](props.uid, z.some).runAsync
              )

          val menuItems = List(
            MenuItem.Custom(
              CheckboxView(
                id = "ags-candidates".refined,
                value = agsCandidatesView,
                label = "Show Catalog"
              )
            ),
            MenuItem.Custom(
              CheckboxView(
                id = "ags-overlay".refined,
                value = agsOverlayView,
                label = "AGS"
              )
            ),
            MenuItem.Custom(
              CheckboxView(
                id = "science-offsets".refined,
                value = scienceOffsetsView,
                label = "Sci. Offsets"
              )
            ),
            MenuItem.Custom(
              CheckboxView(
                id = "acq-offsets".refined,
                value = acquisitionOffsetsView,
                label = "Acq. Offsets"
              )
            ),
            MenuItem.Separator,
            MenuItem.Custom(
              SliderView(
                id = "saturation".refined,
                label = "Saturation",
                clazz = ExploreStyles.AladinRangeControl,
                value = saturationView
                  .zoom(unsafeRangeLens)
              )
            ),
            MenuItem.Custom(
              SliderView(
                id = "brightness".refined,
                label = "Brightness",
                clazz = ExploreStyles.AladinRangeControl,
                value = brightnessView.zoom(unsafeRangeLens)
              )
            ),
            MenuItem.Separator,
            MenuItem.Custom(
              CheckboxView(
                id = "allow-zoom".refined,
                value = allowMouseZoomView.as(AladinMouseScroll.Value),
                label = "Scroll to zoom"
              )
            )
          )

          PopupMenu(model = menuItems, clazz = ExploreStyles.AladinSettingsMenu)
            .withRef(props.menuRef.ref)
      }
