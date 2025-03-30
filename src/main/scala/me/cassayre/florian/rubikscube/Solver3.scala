package me.cassayre.florian.rubikscube

import scala.collection.View

object Solver3 {
  private val Four: Int = 4

  private val Face: Int = 2
  private val BottomColor: Color = RubiksCubeLayout.Colors(Vec(0, 0, -1))

  def apply(cube: RubiksCube): RubiksCube = {
    require(cube.size == 3)
    printDebug("Initial")
      .andThen(solveFirstFace)
      .andThen(printDebug("After first face"))
      .apply(cube)
  }

  private def solveFirstFace(cube: RubiksCube): RubiksCube = {
    def solveFirstFaceRecursive(cube: RubiksCube): RubiksCube = {
      evaluateFirstFace(cube) match
        case Some(seq) =>
          println(seq)
          println(cube)
          val better = exploreAll(4)
            .map(moves => cube.turns(moves.map(_.generalize)))
            .filter(cube => isGreater(evaluateFirstFace(cube), Some(seq)))
            .head
          solveFirstFaceRecursive(better)
        case None => cube
    }
    solveFirstFaceRecursive(cube)
  }

  private def isGreater(a: Option[Seq[Int]], b: Option[Seq[Int]]): Boolean = {
    (a, b) match
      case (Some(av), Some(bv)) => av.zip(bv).find(_ != _).exists(_ > _)
      case (None, Some(_)) => true
      case (_, None) => false
  }

  private def evaluateFirstFace(cube: RubiksCube): Option[Seq[Int]] = {
    // (cross, available_edge, corners, available_cross)
    def booleanToInt(b: Boolean): Int = if b then 1 else 0
    val sides = cross.flatMap((x, y) => (-1 to 1 by 2).map(i => Vec(x * Face + (1 - x.abs) * i, y * Face + (1 - y.abs) * i, 0)))
    val a = evaluateFirstCross(cube)
    val b =
      cross.exists((x, y) => cube(Vec(x, y, Face)) == BottomColor) ||
        sides.exists(v => cube(v) == BottomColor)
    val c = evaluateFirstCorners(cube)
    val d = sides.map(_.copy(z = 1)).exists(v => cube(v) == BottomColor)
    if a < Four || c < Four then
      Some(Seq(a, booleanToInt(b), c, booleanToInt(d)))
    else
      None
  }

  private def evaluateFirstCross(cube: RubiksCube): Int =
    cross.count((x, y) => cube(Vec(x, y, -Face)) == BottomColor && (-1 to 0).map(i => cube(Vec(x * Face, y * Face, i))).toSet.sizeIs == 1)

  private def evaluateFirstCorners(cube: RubiksCube): Int =
    corners.count((x, y) => cube(Vec(x, y, -Face)) == BottomColor && cube(Vec(x * Face, y, -1)) == cube(Vec(x * Face, 0, 0)) && cube(Vec(x, y * Face, -1)) == cube(Vec(0, y * Face, 0)))

  private def exploreAll(depth: Int): Seq[Seq[RubiksCube3Move]] = {
    def exploreRecursive(depth: Int, acc: Seq[Seq[RubiksCube3Move]]): Seq[Seq[RubiksCube3Move]] =
      if depth > 0 then exploreRecursive(depth - 1, acc.flatMap(a => RubiksCube3Move.All.map(_ +: a))) else acc
    exploreRecursive(depth, Seq(Seq.empty))
  }

  private val cross: Set[(Int, Int)] = {
    val range = -1 to 1
    for
      i <- range.toSet
      j <- range
      if i * j == 0 && i + j != 0
    yield (i, j)
  }

  private val corners: Set[(Int, Int)] = {
    val range = -1 to 1 by 2
    for
      i <- range.toSet
      j <- range
    yield (i, j)
  }

  private def printDebug(message: String)(cube: RubiksCube): RubiksCube = {
    println(s"### $message")
    println(cube)
    println()
    cube
  }
}
