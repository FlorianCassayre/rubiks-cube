package me.cassayre.florian.rubikscube

object Main {
  @main
  def main(): Unit = {
    val cube = RubiksCube(3)
    println(cube)
  }
}
