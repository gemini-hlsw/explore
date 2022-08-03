// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq

enum ConfigEditState {
  case TableView, DetailsView, SimpleEdit, AdvancedEdit
}

object ConfigEditState {
  implicit val eqConfigEditState: Eq[ConfigEditState] = Eq.instance {
    case (TableView, TableView)       => true
    case (DetailsView, DetailsView)   => true
    case (SimpleEdit, SimpleEdit)     => true
    case (AdvancedEdit, AdvancedEdit) => true
    case _                            => false
  }
}
