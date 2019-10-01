package model

abstract class Type {
  var `const`: Boolean = false
  def toConst: Type = {
    `const` = true
    this
  }
  val code: Ins.T
  def getSize: Int
}

case object CXInt extends Type {
  val code = Ins.i
  override def getSize: Int = 1
}

case object CXReal extends Type {
  val code = Ins.c
  override def getSize: Int = 1
}

case class CXAddr(baseType: Type) extends Type {
  val code = Ins.a
  override def getSize: Int = 1
}

case object CXBool extends Type {
  val code = Ins.b
  override def getSize: Int = 1
}

case class CXArray(baseType: Type, shape: List[Int]) extends Type {
  val code = baseType.code
  override def getSize: Int = shape.product
}
