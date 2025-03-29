package me.cassayre.florian.rubikscube

import scala.collection.View

object Solver3 {
  private val Four: Int = 4

  private val Face: Int = 2
  private val BottomColor: Color = RubiksCubeLayout.Colors(Vec(0, 0, -1))

  def apply(cube: RubiksCube): RubiksCube = {
    require(cube.size == 3)
    printDebug("Initial")
      .andThen(solveFirstCross)
      .andThen(printDebug("After first cross"))
      .andThen(solveFirstCorners)
      .andThen(printDebug("After first corners"))
      .apply(cube)
  }

  private def solveFirstCross(cube: RubiksCube): RubiksCube = {
    def solveFirstCrossRecursive(cube: RubiksCube): RubiksCube = {
      val count = evaluateFirstCross(cube)
      if count == Four then
        cube
      else
        val better = exploreAll(4).view.map(moves => cube.turns(moves.map(_.generalize))).filter(cube => evaluateFirstCross(cube) > count).head
        solveFirstCrossRecursive(better)
    }
    solveFirstCrossRecursive(cube)
  }

  private def evaluateFirstCross(cube: RubiksCube): Int =
    cross.count((x, y) => cube(Vec(x, y, -Face)) == BottomColor && (-1 to 0).map(i => cube(Vec(x * Face, y * Face, i))).toSet.sizeIs == 1)

  private def solveFirstCorners(cube: RubiksCube): RubiksCube = {
    require(evaluateFirstCross(cube) == Four)
    def solveFirstCornersRecursive(cube: RubiksCube): RubiksCube = {
      val count = evaluateFirstCorners(cube)
      if count == Four then
        cube
      else
        // Incorrect, can require more
        val better = View(4, 5).flatMap(exploreAll).map(moves => cube.turns(moves.map(_.generalize))).filter(cube => evaluateFirstCross(cube) == Four).filter(cube => evaluateFirstCorners(cube) > count).head
        solveFirstCornersRecursive(better)
    }
    solveFirstCornersRecursive(cube)
  }

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
