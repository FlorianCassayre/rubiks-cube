package me.cassayre.florian.rubikscube

object RubiksCubeLayout {
  val Colors: Map[Vec, Color] = Map(
    Vec(1, 0, 0) -> Color.Red,
    Vec(-1, 0, 0) -> Color.Orange,
    Vec(0, 1, 0) -> Color.Blue,
    Vec(0, -1, 0) -> Color.Green,
    Vec(0, 0, 1) -> Color.White,
    Vec(0, 0, -1) -> Color.Yellow,
  )
  val ColorsReverse: Map[Color, Vec] = Colors.map(_.swap)

  val Faces: Set[Vec] = Colors.keySet

  assert(Colors.values.toSet == Color.values.toSet)
  assert(Colors.keySet.forall(_.toSeq.count(_ != 0) == 1))
  assert(Colors.keySet.forall(_.toSeq.forall(_.abs <= 1)))
}
