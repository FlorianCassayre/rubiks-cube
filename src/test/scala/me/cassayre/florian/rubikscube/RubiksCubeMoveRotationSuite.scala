package me.cassayre.florian.rubikscube

import org.scalatest.funsuite.AnyFunSuite

class RubiksCubeMoveRotationSuite extends AnyFunSuite {
  test("cube in a cube") {
    // Source: https://ruwix.com/the-rubiks-cube/rubiks-cube-patterns-algorithms/
    assert(
      """   GGG
        |   GWW
        |   GWW
        |YYYRGGRRWOOO
        |OOYRGGRRWOBB
        |OOYRRRWWWOBB
        |   BBB
        |   YYB
        |   YYB""".stripMargin === RubiksCube(3).turns(RubiksCube3Move.parse("F L F U' R U F2 L2 U' L' B D' B' L2 U").map(_.generalize)).toStringAscii)
  }
}
