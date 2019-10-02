package model

abstract class Stmt extends Node


case class WriteStmt(expr: Expr) extends Stmt {
  override def gen(implicit env: Env): String = expr.gen(env) + Ins.out(expr.tp.code) +
    Ins.ldc(Ins.c, "'\\n'") + Ins.out(Ins.c)
}
case class ReadStmt(identifier: Identifier) extends Stmt {
  override def gen(implicit env: Env): String = Ins.in(identifier.tp.code)
}

case class CompoundStmt(stmts: List[Stmt]) extends Stmt {
  override def gen(implicit env: Env): String = {
    env.symbolTable.openEnv()
    val res = stmts.map(_.gen).mkString
    env.symbolTable.closeEnv()
    res
  }
}
abstract case class SelectionStmt(expr: Expr, stmt1: Stmt, stmt2: Option[Stmt]) extends Stmt

case class DeclarationStmt(tp: Type, as: List[(Identifier, Option[Expr])]) extends Stmt {
  override def gen(implicit env: Env): String = {
    as.map {
      case (identifier, maybeExpr) =>
        env.symbolTable.registerIdentifier(identifier, tp)
        maybeExpr.fold("")(_.gen + Ins.str(tp.code, 0, env.symbolTable.getIdentifier(identifier).get._2))
    }.mkString
  }
}

case object EmptyStmt extends Stmt {
  override def gen(implicit env: Env): String = ""
}

case class ExprStmt(expr: Expr) extends Stmt {
  override def gen(implicit env: Env): String = expr.gen + Ins.pop
}

case class IfStmt(expr: Expr, s1: Stmt, s2: Stmt) extends Stmt {
  override def gen(implicit env: Env): String = {
    val elseLabel = Ins.createLabel
    val endLabel = Ins.createLabel
    expr.gen +
      (if (!expr.tp.isInstanceOf[CXBool]) Ins.conv(expr.tp.code, Ins.b) else "") +
      Ins.fjp(elseLabel) + s1.gen + Ins.ujp(endLabel) +
      Ins.label(elseLabel) + s2.gen + Ins.label(endLabel)
  }
}

case class ReturnStmt(ret: Option[Expr]) extends Stmt {
  override def gen(implicit env: Env): String = ret match {
    case Some(e) => e.gen + Ins.str(e.tp.code, 0, 0) + Ins.retf
    case None => Ins.retp
  }
}