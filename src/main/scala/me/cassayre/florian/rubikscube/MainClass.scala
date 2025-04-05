package me.cassayre.florian.rubikscube

import scala.util.Random

object MainClass {
  private val Size = 3

  @main
  def main(): Unit = {
    given random: Random = new Random(26)
    val cube = RubiksCubeRandomizer.randomized(RubiksCube(Size), 1000)
    val solved = Solver3(cube)
    //println(solved)
  }

}
