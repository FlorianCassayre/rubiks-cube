package me.cassayre.florian.rubikscube

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class Solver3Suite extends AnyFunSuite {
  test("solve a random cube") {
    given random: Random = new Random(2) // Fast first layer

    val initialCube = RubiksCube(3)
    val cube = RubiksCubeRandomizer.randomized(initialCube, 100)
    val solved = Solver3(cube)

    assert(solved === initialCube)
  }
}
