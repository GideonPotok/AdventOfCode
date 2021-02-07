package org.potok.advent

case class Point(x: Int, y:Int){}

case class State(shipPosition:Point, waypointOffset:Point ){}
trait Instruction {
      def raw:String
      def exec(state: State): State
      override def toString:String=raw

}


case class WaypointMovement(xDelta: Int, yDelta: Int, raw:String) extends Instruction {
      override def exec(state: State): State =
            State(state.shipPosition, Point(state.waypointOffset.x + xDelta, state.waypointOffset.y + yDelta))
}
case class WaypointTurn(delta: Int, raw:String) extends Instruction {
      def rotate(pt: Point, rotation:Int): Point = {
            val sameSigns = (pt.x<0) == (pt.y<0)
            rotation match {
                  case 0 => pt
                  case 90 => Point(pt.y, -1*pt.x)
                  case 180 => rotate(rotate(pt, 90), 90)
                  case 270 => rotate(rotate(pt, 180), 90)
}
}
      override def exec(state: State): State =
       State(state.shipPosition, rotate(state.waypointOffset, delta))
}

case class ShipMovement(nTimes: Int, raw:String) extends Instruction {
      override def exec(state: State): State = State(
            Point(
                  state.shipPosition.x + (nTimes * state.waypointOffset.x),
                  state.shipPosition.y + (nTimes * state.waypointOffset.y)
            ),
                  state.waypointOffset
            )}

object Instruction {
  def apply(s: String): Instruction =
{
      val magnitude = s.tail.toInt
      s.head match {
      case 'N'  => WaypointMovement(0, magnitude,s) // Action N means to move north by the given value.
      case 'S'  =>WaypointMovement(0, -1 * magnitude,s) // Action S means to move south by the given value.
      case 'E'  => WaypointMovement( magnitude, 0,s ) // Action E means to move east by the given value.
      case 'W' => WaypointMovement( -1 * magnitude, 0,s ) //Action W means to move west by the given value.
      case 'L' => WaypointTurn(360 - magnitude,s) //Action L means to turn left the given number of degrees.
      case 'R'  => WaypointTurn(magnitude,s) //Action R means to turn right the given number of degrees.
      case 'F'  => ShipMovement(magnitude,s) // Action F means to move forward by the given value in the direction the ship is currently facing.
    }

}
}