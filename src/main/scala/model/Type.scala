package model

abstract class Type {
  var `const`: Boolean = false
  def toConst: Type = {
    `const` = true
    this
  }
  val code: Char
  def getSize: Int
}

class CXInt extends Type {
  val code = 'i'
  override def getSize: Int = 1
}

class CXReal extends Type {
  val code = 'r'
  override def getSize: Int = 1
}

class CXAddr(baseType: Type) extends Type {
  val code = 'a'
  override def getSize: Int = 1
}

class CXBool extends Type {
  val code = 'b'
  override def getSize: Int = 1
}

class CXArray(baseType: Type, shape: List[Int]) extends Type {
  val code = baseType.code
  override def getSize: Int = shape.product
}
