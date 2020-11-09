package explore.model

import eu.timepit.refined.types.string.NonEmptyString

import eu.timepit.refined.auto._

trait Constants {
  val UnnamedTarget: NonEmptyString = "<UNNAMED>"
}

object Constants extends Constants
