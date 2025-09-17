// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.model.Help
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.MultiSelect
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.SelectItem
import lucuma.react.primereact.TooltipOptions
import lucuma.ui.primereact.EnumGroupedMultiSelectView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

import scala.scalajs.js.JSConverters.*

final case class CustomizableEnumGroupedMultiSelect[A](
  id:                       NonEmptyString,
  view:                     View[List[A]],
  groupFunctions:           NonEmptyList[(String, A => Boolean)],
  defaultValue:             List[A],
  defaultFormatter:         Option[A => String] = None, // if you don't want to use the short name
  error:                    Option[String] = None,
  tooltip:                  Option[String] = None,
  tooltipOptions:           Option[TooltipOptions] = None,
  itemTemplate:             Option[SelectItem[A] => VdomNode] = None,
  maxSelectedLabels:        Option[Int] = None,
  selectedItemsLabel:       Option[String] = None,
  displayStyle:             MultiSelect.Display = MultiSelect.Display.Chip,
  filter:                   Boolean = true,             // show filter texbox at top
  showSelectAll:            Boolean = false,
  showClear:                Boolean = true,
  disabled:                 Boolean,
  showCustomization:        Boolean,
  allowRevertCustomization: Boolean,
  exclude:                  Set[A] = Set.empty[A],
  label:                    Option[String] = None,
  helpId:                   Option[Help.Id] = None
)(using val display: Display[A], val enumerated: Enumerated[A])
    extends ReactFnProps(CustomizableEnumGroupedMultiSelect.component)

object CustomizableEnumGroupedMultiSelect:
  private def buildComponent[A] = ScalaFnComponent[CustomizableEnumGroupedMultiSelect[A]](props =>
    import props.given

    val formatDefault: A => String = props.defaultFormatter.getOrElse(display.shortName)
    val originalText               = props.defaultValue.map(formatDefault).mkString(", ")

    React.Fragment(
      props.label.map(label => FormLabel(htmlFor = props.id)(label, props.helpId.map(HelpIcon(_)))),
      <.span(
        LucumaPrimeStyles.FormField,
        PrimeStyles.InputGroup,
        ExploreStyles.ConfigurationImagingFilters,
        EnumGroupedMultiSelectView(
          id = props.id,
          value = props.view,
          groupFunctions = props.groupFunctions,
          exclude = props.exclude,
          displayStyle = props.displayStyle,
          showSelectAll = props.showSelectAll,
          showClear = props.showClear,
          error = props.error.orUndefined,
          filter = props.filter,
          disabled = props.disabled,
          itemTemplate = props.itemTemplate.orUndefined,
          maxSelectedLabels = props.maxSelectedLabels.orUndefined,
          selectedItemsLabel = props.selectedItemsLabel.orUndefined,
          tooltip = props.tooltip.orUndefined,
          tooltipOptions = props.tooltipOptions.orUndefined
        ),
        <.span(PrimeStyles.InputGroupAddon,
               CustomizedGroupAddon(originalText,
                                    props.view.set(props.defaultValue),
                                    props.allowRevertCustomization
               )
        ).when(props.showCustomization && props.view.get =!= props.defaultValue)
      )
    )
  )

  private val component = buildComponent[Any]
