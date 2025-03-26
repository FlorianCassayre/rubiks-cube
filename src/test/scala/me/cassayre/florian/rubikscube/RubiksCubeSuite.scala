package me.cassayre.florian.rubikscube

import org.scalatest.funsuite.AnyFunSuite

class RubiksCubeSuite extends AnyFunSuite {
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

  test("neat pattern size 3") {
    assert(
      """   WYW
        |   YWY
        |   WYW
        |OROGBGRORBGB
        |RORBGBOROGBG
        |OROGBGRORBGB
        |   YWY
        |   WYW
        |   YWY""".stripMargin === RubiksCube(3).turn(Vec(1, 0, 0), 1, 2).turn(Vec(0, 1, 0), 1, 2).turn(Vec(0, 0, 1), 1, 2).toStringAscii)
  }

  test("turn identity") {
    for {
      size <- 3 to 3
      turns <- Seq(0, 4, -4)
      layer <- 0 until size
      axis <- RubiksCubeLayout.Faces
    } {
      val cube = RubiksCube(size)
      assert(cube === cube.turn(axis, layer, turns))
    }
  }

  test("simple periodic pattern") {
    val initial = RubiksCube(3)
    val indices = LazyList.iterate(initial)(_
      .turn(Vec(1, 0, 0), 0, 1)
      .turn(Vec(0, 1, 0), 0, -1)
      .turn(Vec(1, 0, 0), 0, -1)
      .turn(Vec(0, 1, 0), 0, 1)
    ).zipWithIndex.filter(_._1 == initial).map(_._2)
    assert(6 === indices.tail.head)
  }

  test("3 moves") {
    // Dumped this snapshot after verifying each turn individually, avoid writing 3 tests for it
    assert("""   BWW
             |   BWW
             |   OOO
             |OOGWWWBRRBBY
             |OOYGGGWRRBBY
             |BBYOOYGGGWRR
             |   GGR
             |   YYR
             |   YYR""".stripMargin === RubiksCube(3).turn(Vec(1, 0, 0), 0, 1).turn(Vec(0, 1, 0), 0, 1).turn(Vec(0, 0, 1), 0, 1).toStringAscii)
  }
}
