// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.ExploreStyles
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes.Small

object Icons {
  val Bars                = Icon("bars")
  val Home                = Icon("home")
  val Cogs                = Icon("cogs")
  val Undo                = Icon("undo alternate")
  val Redo                = Icon("redo alternate")
  val New                 = Icon("plus")
  val Delete              = Icon("trash alternate outline")
  val Bullseye            = Icon("bullseye")
  val Crosshairs          = Icon("crosshairs")
  val UserCircle          = Icon("user circle")
  val Lock                = Icon("lock")
  val User                = Icon("user")
  val CircleNotched       = Icon("circle notched")
  val Edit                = Icon("edit")
  val Search              = Icon("search")
  val ChevronRight        = Icon("chevron right")
  val ChevronLeft         = Icon("chevron left")
  val Sort                = Icon("sort")
  val ExclamationTriangle = Icon("exclamation triangle")
  val Ban                 = Icon("ban")
  val UserAstronaut       = Icon(className = "user-astronaut")
  val Logout              = Icon("sign out alternate")
  val SkullCrossBones     = Icon(className = "skull crossbones")
  val WarningSign         = Icon("warning sign")
  val Info                = Icon("info circle")
  val Clipboard           = Icon("clipboard outline")
  val ClipboardCheck      = Icon("clipboard check")
  val Close               = Icon("close")
  val MousePointer        = Icon("mouse pointer")
  val Expand              = Icon("expand")
  val Minimize            = Icon(className = "compress alt")
  val Maximize            = Icon(className = "expand alt")
  val Checkmark           = Icon("checkmark")
  val Remove              = Icon("remove")
  val Trash               = Delete.size(Small).fitted(true).clazz(ExploreStyles.TrashIcon)
  val Copy                = Icon("copy")
  val List                = Icon("list alternate outline")
  val Question            = Icon("question circle")
}
