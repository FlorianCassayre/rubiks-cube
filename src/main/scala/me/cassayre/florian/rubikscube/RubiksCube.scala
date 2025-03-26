package me.cassayre.florian.rubikscube

object RubiksCube {
  def apply(size: Int): RubiksCube = {
    require(size > 0)
    val r = range(size)
    val (min, max) = (r.min - 1, r.max + 1)
    val pieces = RubiksCubeLayout.Faces.flatMap { v =>
      val d = if v.toSeq.exists(_ > 0) then v * max else -v * min
      val indices = v.toSeq.zipWithIndex.collect { case (value, i) if value == 0 => i }
      val face = r.flatMap(y => r.map(x => Vec(indices.zip(Seq(x, y)).foldLeft(d.toSeq) { case (vec, (i, value)) => vec.updated(i, value) })))
      face.map(_ -> RubiksCubeLayout.Colors(v))
    }
    new RubiksCube(size, pieces.toMap)
  }

  private def range(size: Int): Range = -size / 2 until (size + 1) / 2
}

case class RubiksCube private (size: Int, private val pieces: Map[Vec, Color]) {
  def hasCenter: Boolean = size % 2 != 0
  def hasCorners: Boolean = size > 1
  def edgesCount: Int = if size > 2 then size - 2 else 0
  def hasEdges: Boolean = edgesCount > 0

  private def to2d: Seq[Seq[Option[Color]]] =
    val range = RubiksCube.range(size)
    val (min, max) = (range.min - 1, range.max + 1)
    def face(xyToVec: (Int, Int) => Vec): IndexedSeq[IndexedSeq[Color]] =
      range.map(y => range.map(x => pieces(xyToVec(x, y))))
    def invertRange(v: Int): Int = range.min + range.max - v
    val white = face((x, y) => Vec(x, invertRange(y), max))
    val orange = face((x, y) => Vec(min, invertRange(y), x))
    val green = face((x, y) => Vec(x, min, invertRange(y)))
    val red = face((x, y) => Vec(max, x, invertRange(y)))
    val blue = face((x, y) => Vec(invertRange(x), max, invertRange(y)))
    val yellow = face((x, y) => Vec(x, y, min))
    Seq(
      white.map(Seq.fill(size)(None) ++ _.map(Some.apply)),
      Seq(orange, green, red, blue).reduce((a, b) => a.zip(b).map(_ ++ _)).map(_.map(Some.apply)),
      yellow.map(Seq.fill(size)(None) ++ _.map(Some.apply)),
    ).flatten

  private def toString(colored: Boolean): String =
    to2d.map(_.map(_.map(c => if colored then c.toAnsiColoredChar else c.toChar).getOrElse(" ")).mkString).mkString("\n")

  override def toString: String = toString(true)

  def toStringAscii: String = toString(false)
}
