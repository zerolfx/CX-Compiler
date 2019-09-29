package model

abstract class Stmt extends Node


case class WriteStmt(expr: Expr) extends Stmt {
  override def gen: String = expr.gen + Ins.out(expr.tp.code)
}
case class ReadStmt(identifier: Identifier) extends Stmt {
  override def gen: String = Ins.in(identifier.tp.code)
}

case class CompoundStmt(stmts: List[Stmt]) extends Stmt {
  override def gen: String = stmts.map(_.gen).mkString
}
abstract case class SelectionStmt(expr: Expr, stmt1: Stmt, stmt2: Option[Stmt]) extends Stmt

case class DeclarationStmt(tp: Type, as: List[(Identifier, Option[Expr])]) extends Stmt {
  override def gen: String = ???
}

case class EmptyStmt() extends Stmt {
  override def gen: String = ""
}