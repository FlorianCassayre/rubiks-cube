package me.cassayre.florian.rubikscube

object Solver3 {
  private val Four: Int = 4

  private val Face: Int = 2
  private val Z: Vec = RubiksCube3MoveType.Up.axis
  private val BottomColor: Color = RubiksCubeLayout.Colors(-Z)
  private val TopColor: Color = RubiksCubeLayout.Colors(Z)

  def apply(cube: RubiksCube): RubiksCube = {
    require(cube.size == 3)
    printDebug("Initial")
      .andThen(solveFirstFace)
      .andThen(printDebug("After first face"))
      .andThen(solveSecondLayer)
      .andThen(printDebug("After second layer"))
      .andThen(solveThirdLayerEdgeOrientation)
      .andThen(printDebug("After third layer edge orientation"))
      .andThen(solveThirdLayerEdgePlacement)
      .andThen(printDebug("After third layer edge placement"))
      .apply(cube)
  }

  private def solveThirdLayerEdgePlacement(cube: RubiksCube): RubiksCube = {
    val algorithm = RubiksCube3Move.parse("R U R' U R U2 R' U2")
    def solveThirdLayerEdgePlacementRecursive(cube: RubiksCube): RubiksCube = {
      val placements = cross.map((x, y) => Vec(x, y, 0) -> cube(Vec(x * Face, y * Face, 1)))
      val targetPlacement = RubiksCubeLayout.Colors.view.filterKeys(_.z == 0).toMap
      val rotations = RubiksCubeMoveRotation.All.map(_.map(_.turns).getOrElse(0))
      val correctlyPlaced = rotations.map(turns =>
        turns -> placements.collect { case (face, color) if targetPlacement(Z.withRotation(turns)(face)) == color => face }
      )
      val solved = correctlyPlaced.collectFirst { case (turns, placed) if placed.sizeIs == Four => turns }
      solved match {
        case Some(turns) =>
          cube.turns(RubiksCubeMoveRotation.fromTurns(turns).map(r => RubiksCube3Move(RubiksCube3MoveType.Up, r)).map(_.generalize))
        case None =>
          // Find the first color with exactly one edge placed; order by ordinal to avoid infinite cycles
          val rotation = correctlyPlaced.collect { case (turns, placed) if placed.sizeIs == 1 => turns -> placed.head }
            .toSeq.minByOption((_, face) => RubiksCubeLayout.Colors(face).ordinal)
            .flatMap((turns, face) =>
              RubiksCubeMoveRotation.fromTurns(
                turns + RubiksCubeMoveRotation.values.find(r => Z.withRotation(r.turns)(face) == RubiksCube3MoveType.Left.axis).map(_.turns).getOrElse(0)
              )
            ).map(_.turns).getOrElse(0)
          solveThirdLayerEdgePlacementRecursive(cube.turns(algorithm.map(_.generalize.rotate(Z, rotation))))
      }
    }
    solveThirdLayerEdgePlacementRecursive(cube)
  }

  private def solveThirdLayerEdgeOrientation(cube: RubiksCube): RubiksCube = {
    val algorithm = RubiksCube3Move.parse("R' U' F' U F R")
    def solveThirdLayerEdgeOrientationRecursive(cube: RubiksCube): RubiksCube = {
      val correctlyPlaced = cross.map((x, y) => Vec(x, y, 0) -> (cube(Vec(x, y, Face)) == TopColor)).toMap
      if correctlyPlaced.values.count(identity) == Four then
        cube
      else
        val firstMove = RubiksCubeMoveRotation.values.find(r => Set(RubiksCube3MoveType.Left, RubiksCube3MoveType.Back).forall(m => correctlyPlaced(Z.withRotation(r.turns)(m.axis))))
          .map(r => RubiksCube3Move(RubiksCube3MoveType.Up, r.inverse))
        solveThirdLayerEdgeOrientationRecursive(cube.turns((firstMove.toSeq ++ algorithm).map(_.generalize)))
    }
    solveThirdLayerEdgeOrientationRecursive(cube)
  }

  private def solveSecondLayer(cube: RubiksCube): RubiksCube = {
    def solveSecondLayerRecursive(cube: RubiksCube): RubiksCube = {
      //println(cube)
      //println()
      // Also indicates whether this edge is not a top edge
      val incorrectlyPlacedRight = cross.flatMap { (x, y) =>
        val frontFace = Vec(x, y, 0)
        val expectedFront = RubiksCubeLayout.Colors(frontFace)
        val rightFace = Z.withRotation(1)(frontFace)
        val expectedRight = RubiksCubeLayout.Colors(rightFace)
        val frontValue = cube(frontFace * Face + rightFace)
        val rightValue = cube(rightFace * Face + frontFace)
        if expectedFront == frontValue && expectedRight == rightValue then
          None
        else
          Some((frontFace, !Set(frontValue, rightValue).contains(TopColor)))
      }.toSeq
      if incorrectlyPlacedRight.isEmpty then
        cube
      else
        // (side, top)
        val topEdges = cross.map((x, y) => (x, y) -> (cube(Vec(x * Face, y * Face, 1)), cube(Vec(x, y, Face))))
        val candidatesTop = topEdges.filter { case (_, (a, b)) => !Set(a, b).contains(TopColor) }.toSeq
        val rightAlgorithm = RubiksCube3Move.parse("U R U' R' U' F' U F") // Assumes the edge color matches the face it's currently on (we can later improve this to save a move)
        val leftAlgorithm = RubiksCube3Move.parse("U' L' U L U F U' F'") // TODO combine those
        val finalAlgorithm = candidatesTop match {
          case ((x, y), (side, top)) +: _ =>
            val targetFace = RubiksCubeLayout.ColorsReverse(side)
            val algorithm = {
              val product = targetFace.cross(RubiksCubeLayout.ColorsReverse(top))
              if product == -Z then leftAlgorithm else if product == Z then rightAlgorithm else throw new IllegalStateException()
            }
            val initialMove = RubiksCubeMoveRotation.values.find(rotation => Z.withRotation(rotation.turns)(Vec(x, y, 0)) == targetFace)
              .map(rotation => RubiksCube3Move(RubiksCube3MoveType.Up, rotation))
            val algorithmRotation = RubiksCubeMoveRotation.values.find(rotation => Z.withRotation(rotation.turns)(RubiksCube3MoveType.Front.axis) == targetFace)
              .map(rotation => rotation.turns).getOrElse(0)
            initialMove.map(_.generalize).toSeq ++ algorithm.map(move => move.generalize.rotate(Z, algorithmRotation))
          case _ =>
            // Normally guaranteed that there is one
            val face = incorrectlyPlacedRight.collect { case (v, true) => v }.head
            val algorithmRotation = RubiksCubeMoveRotation.values.find(rotation => Z.withRotation(rotation.turns)(RubiksCube3MoveType.Front.axis) == face)
              .map(rotation => rotation.turns).getOrElse(0)
            rightAlgorithm.map(move => move.generalize.rotate(Z, algorithmRotation))
        }
        solveSecondLayerRecursive(cube.turns(finalAlgorithm))
    }
    solveSecondLayerRecursive(cube)
  }

  private def solveFirstFace(cube: RubiksCube): RubiksCube = {
    def solveFirstFaceRecursive(cube: RubiksCube): RubiksCube = {
      evaluateFirstFace(cube) match
        case Some(seq) =>
          //println(seq)
          //println(cube)
          val better = (3 to 4).view.flatMap(exploreAll)
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
