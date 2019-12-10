package model

abstract class Type {
  var `const`: Boolean = false
  def toConst: Type = {
    `const` = true
    this
  }
  val code: Ins.T
  def getSize: Int
  def default: String
}

case class CXInt() extends Type {
  val code = Ins.i
  override def getSize: Int = 1
  override def default: String = "0"
}

case class CXReal() extends Type {
  val code = Ins.r
  override def getSize: Int = 1
  override def default: String = "0"
}

case class CXAddr(baseType: Type) extends Type {
  val code = Ins.a
  override def getSize: Int = 1
  override def default: String = "0"
}

case class CXBool() extends Type {
  val code = Ins.b
  override def getSize: Int = 1
  override def default: String = "f"
}

case class CXArray(baseType: Type, shape: List[Int]) extends Type {
  val code = baseType.code
  override def getSize: Int = shape.product
  override def default: String = baseType.default
  private val weight = shape.scanRight(1)((n, s) => n * s).drop(1)
  def genAddr(sub: List[Expr], addr: Int)(implicit env: Env): String = {
    Ins.lda(0, addr) + Ins.conv(Ins.a, Ins.i) +
    sub.zip(weight).map {
      case (expr, w) =>
        expr.gen + Ins.ldc(Ins.i, w.toString) + Ins.mul(Ins.i) + Ins.add(Ins.i)
    }.mkString +
    Ins.conv(Ins.i, Ins.a)
  }
}

case object CXVoid extends Type {
  val code = Ins.v
  override def getSize: Int = 0
  override def default: String = ???
}