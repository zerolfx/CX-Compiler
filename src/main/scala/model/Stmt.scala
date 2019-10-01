package model

abstract class Stmt extends Node


case class WriteStmt(expr: Expr) extends Stmt {
  override def gen(implicit env: Env): String = expr.gen(env) + Ins.out(expr.tp.code)
}
case class ReadStmt(identifier: Identifier) extends Stmt {
  override def gen(implicit env: Env): String = Ins.in(identifier.tp.code)
}

case class CompoundStmt(stmts: List[Stmt]) extends Stmt {
  override def gen(implicit env: Env): String = {
    val newEnv = env.openEnv()
    val res = stmts.map(_.gen(newEnv)).mkString
    newEnv.closeEnv()
    res
  }
}
abstract case class SelectionStmt(expr: Expr, stmt1: Stmt, stmt2: Option[Stmt]) extends Stmt

case class DeclarationStmt(tp: Type, as: List[(Identifier, Option[Expr])]) extends Stmt {
  override def gen(implicit env: Env): String = {
    as.map {
      case (identifier, maybeExpr) =>
        env.registerIdentifier(identifier, tp)
        maybeExpr.fold("")(_.gen + Ins.str(tp.code, 0, env.getIdentifier(identifier).get._2))
    }.mkString
  }
}

case object EmptyStmt extends Stmt {
  override def gen(implicit env: Env): String = ""
}

case class ExprStmt(expr: Expr) extends Stmt {
  override def gen(implicit env: Env): String = expr.gen + Ins.pop
}