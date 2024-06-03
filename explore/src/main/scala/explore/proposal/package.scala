// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import lucuma.core.util.NewType

object PartnersDialogState extends NewType[Boolean]:
  val Shown: PartnersDialogState  = PartnersDialogState(true)
  val Hidden: PartnersDialogState = PartnersDialogState(false)

type PartnersDialogState = PartnersDialogState.Type
