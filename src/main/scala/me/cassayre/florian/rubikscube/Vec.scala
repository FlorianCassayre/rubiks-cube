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
  def toSeq: IndexedSeq[Int] = IndexedSeq(x, y, z)
}
