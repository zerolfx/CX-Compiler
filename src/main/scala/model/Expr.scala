package model

trait withType {
  var tp: Type
}

abstract class Expr extends Stmt with withType

case class Num(numberLit: String) extends Expr {
  override var tp: Type = numberLit.toIntOption.fold(CXReal: Type)(_ => CXInt)

  override def gen: String = Ins.ldc(tp.code, numberLit)
}

case class BinaryOp(op: String, var left: Expr, right: Expr) extends Expr {
  override var tp: Type = if (left.tp == right.tp) left.tp else throw new Exception("hhh")

  override def gen: String = left.gen + right.gen + (op match {
    case "+" => Ins.add(tp.code)
    case "-" => Ins.sub(tp.code)
    case "*" => Ins.mul(tp.code)
    case _ => throw new Exception(op)
  })
}

abstract class Identifier extends Expr with withType

case class ArrayIdentifier(name: String, sub: List[Expr]) extends Identifier {
  override def gen: String = ???

  override var tp: Type = _
}

case class SingleIdentifier(name: String) extends Identifier {
  override def gen: String = ???

  override var tp: Type = _
}

case class UnaryOp(op: String, expr: Expr) extends Expr {
  override def gen: String = ???

  override var tp: Type = _
}

case class CastExpr(var tp: Type, expr: Expr) extends Expr {
  override def gen: String = ???
}

case class AssignExpr(id: Identifier, expr: Expr) extends Expr {
  override def gen: String = ???

  override var tp: Type = _
}