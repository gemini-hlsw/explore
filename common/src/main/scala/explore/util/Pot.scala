package explore.util

import japgolly.scalajs.react._
import diode.data._

object Pot {
  implicit def potReuse[A: Reusability]: Reusability[Pot[A]] =
    Reusability((x, y) =>
      x match {
        case Empty =>
          y match {
            case Empty => true
            case _     => false
          }
        case Unavailable =>
          y match {
            case Unavailable => true
            case _           => false
          }
        case Ready(a) =>
          y match {
            case Ready(b) => a ~=~ b
            case _        => false
          }
        case Pending(t) =>
          y match {
            case Pending(s) => t ~=~ s
            case _          => false
          }
        case PendingStale(a, t) =>
          y match {
            case PendingStale(b, s) => a ~=~ b && t ~=~ s
            case _                  => false
          }
        case Failed(e) =>
          y match {
            case Failed(f) => e.getMessage ~=~ f.getMessage
            case _         => false
          }
        case FailedStale(a, e) =>
          y match {
            case FailedStale(b, f) => a ~=~ b && e.getMessage ~=~ f.getMessage
            case _                 => false
          }
      }
    )  
}