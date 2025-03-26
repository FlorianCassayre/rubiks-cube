package me.cassayre.florian.rubikscube

import org.scalatest.funsuite.AnyFunSuite

class RubiksCubeSpec extends AnyFunSuite {
  test("toString on size 1") {
    assert(
      """ W
        |OGRB
        | Y""".stripMargin === RubiksCube(1).toStringAscii)
  }

  test("toString on size 2") {
    assert(
      """  WW
        |  WW
        |OOGGRRBB
        |OOGGRRBB
        |  YY
        |  YY""".stripMargin === RubiksCube(2).toStringAscii)
  }

  test("toString on size 3") {
    assert(
      """   WWW
        |   WWW
        |   WWW
        |OOOGGGRRRBBB
        |OOOGGGRRRBBB
        |OOOGGGRRRBBB
        |   YYY
        |   YYY
        |   YYY""".stripMargin === RubiksCube(3).toStringAscii)
  }

  test("toString on size 4") {
    assert(
      """    WWWW
        |    WWWW
        |    WWWW
        |    WWWW
        |OOOOGGGGRRRRBBBB
        |OOOOGGGGRRRRBBBB
        |OOOOGGGGRRRRBBBB
        |OOOOGGGGRRRRBBBB
        |    YYYY
        |    YYYY
        |    YYYY
        |    YYYY""".stripMargin === RubiksCube(4).toStringAscii)
  }
}
