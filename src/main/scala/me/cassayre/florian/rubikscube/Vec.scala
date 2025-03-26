package me.cassayre.florian.rubikscube

object Vec {
  def apply(seq: Seq[Int]): Vec = seq match {
    case Seq(x, y, z) => Vec(x, y, z)
    case _ => assert(false)
  }
}

case class Vec(x: Int, y: Int, z: Int) {
  def unary_- : Vec = Vec(-x, -y, -z)
  inline def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)
  inline def *(v: Int): Vec = Vec(x * v, y * v, z * v)
  def dot(that: Vec): Int = x * that.x + y * that.y + z * that.z
  def cross(that: Vec): Vec = Vec(
    y * that.z - z * that.y,
    z * that.x - x * that.z,
    x * that.y - y * that.x
  )
  def withRotation(quarterTurns: Int): Vec => Vec = {
    require(toSeq.map(_.abs).groupBy(identity).view.mapValues(_.size).toMap == Map(0 -> 2, 1 -> 1))
    val (cos, sin) = ((quarterTurns % 4) + 4) % 4 match {
      case 0 => (1, 0)
      case 1 => (0, 1)
      case 2 => (-1, 0)
      case 3 => (0, -1)
    }
    v => {
      val d = this.dot(v)
      val c = this.cross(v)
      val p = this * d
      val t = v + (this * -d)
      p + (t * cos) + (c * sin)
    }
  }
  def toSeq: IndexedSeq[Int] = IndexedSeq(x, y, z)
}
