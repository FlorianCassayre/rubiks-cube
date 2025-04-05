package me.cassayre.florian.rubikscube

object RubiksCubeMoveRotation {
  val Total: Int = 4
  private val Tokens = RubiksCubeMoveRotation.values.flatMap(e => e.char.map(_ -> e)).toMap
  def parse(s: Char): Option[RubiksCubeMoveRotation] = Tokens.get(s)
  private def normalizeTurns(turns: Int): Int = ((turns % Total) + Total) % Total
  def fromTurns(turns: Int): Option[RubiksCubeMoveRotation] = {
    val normalized = normalizeTurns(turns)
    values.find(r => normalizeTurns(r.turns) == normalized)
  }
}

enum RubiksCubeMoveRotation(val char: Option[Char], val turns: Int) {
  case Clockwise extends RubiksCubeMoveRotation(None, -1)
  case CounterClockwise extends RubiksCubeMoveRotation(Some('\''), 1)
  case Double extends RubiksCubeMoveRotation(Some('2'), -2)
  
  def inverse: RubiksCubeMoveRotation = this match
    case Clockwise => CounterClockwise
    case CounterClockwise => Clockwise
    case Double => Double
}
