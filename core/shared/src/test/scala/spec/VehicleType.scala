//package spec
//
//import fabric.rw.RW
//
//trait VehicleType
//
//object VehicleType {
//  implicit val rw: RW[VehicleType] = RW.gen
//
//  case object Car extends VehicleType
//  case object SUV extends VehicleType
//  case object Truck extends VehicleType
//}