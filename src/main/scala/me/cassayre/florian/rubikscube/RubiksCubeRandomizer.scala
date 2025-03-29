package me.cassayre.florian.rubikscube

import scala.util.Random

object RubiksCubeRandomizer {
  def randomized(cube: RubiksCube, moveCount: Int)(using random: Random): RubiksCube =
    cube.turns(Seq.fill(moveCount)(randomMove(cube.size)))

  def randomMove(size: Int)(using random: Random): RubiksCubeGeneralMove = {
    val faces = RubiksCubeLayout.Faces.toSeq
    RubiksCubeGeneralMove(
      faces(random.nextInt(faces.size)),
      random.nextInt(size / 2), // Enough
      random.nextInt(3) + 1
    )
  }

  def randomMove3(using random: Random): RubiksCube3Move =
    RubiksCube3Move(
      RubiksCube3MoveType.values(random.nextInt(RubiksCube3MoveType.values.length)),
      RubiksCubeMoveRotation.values(random.nextInt(RubiksCubeMoveRotation.values.length))
    )
}
