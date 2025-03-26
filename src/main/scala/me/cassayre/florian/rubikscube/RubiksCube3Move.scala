package me.cassayre.florian.rubikscube

object RubiksCube3Move {
  def parse(s: String): Seq[RubiksCube3Move] = {
    def parseRecursive(tokens: Seq[Char], result: Seq[RubiksCube3Move]): Seq[RubiksCube3Move] = tokens match {
      case a +: b +: tail =>
        val av = RubiksCube3MoveType.parse(a).get
        RubiksCubeMoveRotation.parse(b) match
          case Some(bv) => parseRecursive(tail, RubiksCube3Move(av, bv) +: result)
          case None => parseRecursive(b +: tail, RubiksCube3Move(av) +: result)
      case a +: _ => RubiksCube3Move(RubiksCube3MoveType.parse(a).get) +: result
      case _ => result
    }
    parseRecursive(s.replace(" ", ""), Seq.empty).reverse
  }
}

case class RubiksCube3Move(moveType: RubiksCube3MoveType, rotation: RubiksCubeMoveRotation = RubiksCubeMoveRotation.Clockwise) {
  def generalize: RubiksCubeGeneralMove = RubiksCubeGeneralMove(moveType.axis, 0, rotation.turns)
}
