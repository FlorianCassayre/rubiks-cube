package me.cassayre.florian.rubikscube

case class RubiksCubeGeneralMove(axis: Vec, layer: Int, turns: Int) {
  def inverse: RubiksCubeGeneralMove = copy(turns = -turns)
  def rotate(rotationAxis: Vec, rotationTurns: Int): RubiksCubeGeneralMove =
    copy(axis = rotationAxis.withRotation(rotationTurns)(axis))
}
