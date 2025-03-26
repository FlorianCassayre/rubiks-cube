package me.cassayre.florian.rubikscube

object Main {
  @main
  def main(): Unit = {
    val cube = RubiksCube(3)
    println(cube)
    println()
    println(cube.turn(Vec(1, 0, 0), 1, 3))
  }
}
