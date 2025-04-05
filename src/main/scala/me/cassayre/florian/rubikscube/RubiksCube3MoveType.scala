package me.cassayre.florian.rubikscube

object RubiksCube3MoveType {
  private val Tokens = values.map(e => e.char -> e).toMap
  private val Axis = values.map(e => e.axis -> e).toMap
  def parse(s: Char): Option[RubiksCube3MoveType] = Tokens.get(s)
  def from(axis: Vec): RubiksCube3MoveType = Axis(axis)
}

enum RubiksCube3MoveType(val char: Char, val axis: Vec) {
  case Up extends RubiksCube3MoveType('U', Vec(0, 0, 1))
  case Down extends RubiksCube3MoveType('D', Vec(0, 0, -1))
  case Left extends RubiksCube3MoveType('L', Vec(-1, 0, 0))
  case Right extends RubiksCube3MoveType('R', Vec(1, 0, 0))
  case Front extends RubiksCube3MoveType('F', Vec(0, -1, 0))
  case Back extends RubiksCube3MoveType('B', Vec(0, 1, 0))
}
