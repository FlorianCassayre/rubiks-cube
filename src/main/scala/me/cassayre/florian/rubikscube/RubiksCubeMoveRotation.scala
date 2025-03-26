package me.cassayre.florian.rubikscube

object RubiksCubeMoveRotation {
  private val Tokens = RubiksCubeMoveRotation.values.flatMap(e => e.char.map(_ -> e)).toMap
  def parse(s: Char): Option[RubiksCubeMoveRotation] = Tokens.get(s)
}

enum RubiksCubeMoveRotation(val char: Option[Char], val turns: Int) {
  case Clockwise extends RubiksCubeMoveRotation(None, -1)
  case CounterClockwise extends RubiksCubeMoveRotation(Some('\''), 1)
  case Double extends RubiksCubeMoveRotation(Some('2'), -2)
}
