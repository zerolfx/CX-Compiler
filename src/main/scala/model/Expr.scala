package model

trait withType {
  var tp: Type
}

abstract class Expr extends Node with withType

case class Num(numberLit: String) extends Expr {
  override var tp: Type =
    if (numberLit == "t" || numberLit == "f") CXBool()
    else numberLit.toIntOption.fold(CXReal(): Type)(_ => CXInt())

  override def gen(implicit env: Env): String = Ins.ldc(tp.code, numberLit)
}

case class BinaryOp(op: String, var left: Expr, right: Expr) extends Expr {
  override var tp: Type = _

  override def gen(implicit env: Env): String = {
    val res = left.gen + right.gen
    tp = if (left.tp == right.tp) left.tp else throw new Exception("hhh")
    val ret = res + (op match {
      case "+" => Ins.add(tp.code)
      case "-" => Ins.sub(tp.code)
      case "*" => Ins.mul(tp.code)
      case "/" => Ins.div(tp.code)
      case "%" => Ins.mod
      case "&&" => Ins.and
      case "||" => Ins.or
      case "==" => Ins.equ(tp.code)
      case ">=" => Ins.geq(tp.code)
      case "<=" => Ins.leq(tp.code)
      case "<" => Ins.les(tp.code)
      case ">" => Ins.grt(tp.code)
      case "!=" => Ins.neq(tp.code)
    })
    if (List("==", "!=", "<=", ">=", "<", ">", "&&", "||").contains(op)) tp = CXBool()
    ret
  }
}

abstract class Identifier extends Expr {
  val name: String
}

case class ArrayIdentifier(name: String, sub: List[Expr]) extends Identifier {
  override def gen(implicit env: Env): String = ???

  override var tp: Type = _
}

case class SingleIdentifier(name: String) extends Identifier {
  override var tp: Type = _

  override def gen(implicit env: Env): String = {
    val (_tp, addr) = env.symbolTable.getIdentifier(this).get
    tp = _tp
    Ins.lod(tp.code, 0, addr)
  }


}

case class UnaryOp(op: String, expr: Expr) extends Expr {
  override def gen(implicit env: Env): String = ???

  override var tp: Type = _
}

case class CastExpr(var tp: Type, expr: Expr) extends Expr {
  override def gen(implicit env: Env): String = expr.gen + Ins.conv(expr.tp.code, tp.code)
}

case class AssignExpr(id: Identifier, expr: Expr) extends Expr {
  override def gen(implicit env: Env): String = {
    val (_tp, addr) = env.symbolTable.getIdentifier(id).get
    val res = expr.gen
    if (_tp.`const`) throw new Exception("assign to const.")
    if (_tp.getClass == expr.tp.getClass) tp = _tp else throw new Exception("type...")
    res + Ins.dpl(tp.code) + Ins.str(tp.code, 0, addr)
  }

  override var tp: Type = _
}

case class FunctionCallExpr(name: String, args: List[Expr]) extends Expr {
  override def gen(implicit env: Env): String = {
    // TODO check arguments
    val fun = env.funTable.getFunction(name).get
    tp = fun._1
    Ins.mst(0) + args.map(_.gen).mkString + Ins.cup(args.map(_.tp.getSize).sum, "fun" + name)
  }

  override var tp: Type = _
}