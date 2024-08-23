// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.model

// import cats.Eq
// import cats.derived.*
// import eu.timepit.refined.cats.given
// import eu.timepit.refined.types.numeric.NonNegInt
// import eu.timepit.refined.types.numeric.PosInt
// import explore.model.itc.ItcExposureTime
// import explore.model.itc.OverridenExposureTime
// import io.circe.Decoder
// import io.circe.generic.semiauto
// import io.circe.refined.*
// import lucuma.core.enums.ObserveClass
// import lucuma.core.math.SignalToNoise
// import lucuma.core.util.TimeSpan
// import lucuma.itc.syntax.*
// import lucuma.odb.json.time.decoder.given
// import monocle.Focus
// import monocle.Prism
// import monocle.macros.GenPrism

// sealed trait OdbItcResult extends Product with Serializable

// // Currently the ODB and ITC have `ItcResult` types, and they don't match.
// // At some point we may unify these, or always go directly to the ITC instead
// // of getting ITC data via the ODB.
// object OdbItcResult {
//   case class Success(
//     sciExposureTime:  TimeSpan,
//     sciExposures:     NonNegInt,
//     sciSignalToNoise: SignalToNoise,
//     acqExposures:     NonNegInt,
//     acqSignalToNoise: SignalToNoise
//   ) extends OdbItcResult
//       derives Eq {
//     def toItcExposureTime: ItcExposureTime =
//       ItcExposureTime(
//         OverridenExposureTime.FromItc,
//         sciExposureTime,
//         PosInt.unsafeFrom(sciExposures.value)
//       )

//     def sciExposureSignalToNoise: Option[SignalToNoise] =
//       PosInt.from(sciExposures.value).toOption.flatMap(sciSignalToNoise.stepSignalToNoise)

//     def acqExposureSignalToNoise: Option[SignalToNoise] =
//       PosInt.from(acqExposures.value).toOption.flatMap(acqSignalToNoise.stepSignalToNoise)

//     val snPerClass: Map[ObserveClass, SignalToNoise] =
//       Map(ObserveClass.Science     -> sciExposureSignalToNoise,
//           ObserveClass.Acquisition -> acqExposureSignalToNoise
//       ).collect { case (k, Some(v)) => k -> v }
//   }

//   case class MissingParams(
//     params: List[String]
//   ) extends OdbItcResult

//   case class ServiceError(
//     message: String
//   ) extends OdbItcResult

//   val success: Prism[OdbItcResult, Success]             = GenPrism[OdbItcResult, Success]
//   val missingParams: Prism[OdbItcResult, MissingParams] = GenPrism[OdbItcResult, MissingParams]
//   val serviceError: Prism[OdbItcResult, ServiceError]   = GenPrism[OdbItcResult, ServiceError]

//   given Decoder[Success]       = semiauto.deriveDecoder
//   given Decoder[MissingParams] = semiauto.deriveDecoder
//   given Decoder[ServiceError]  = semiauto.deriveDecoder

//   given Decoder[OdbItcResult] = Decoder.instance { c =>
//     c.downField("result")
//       .as[Success]
//       .orElse(c.downField("result").as[MissingParams])
//       .orElse(c.downField("result").as[ServiceError])
//   }
// }
