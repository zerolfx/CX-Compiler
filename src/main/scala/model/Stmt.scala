package model

class Stmt


case class WriteStmt(expr: Expr) extends Stmt
case class ReadStmt(name: String) extends Stmt

case class CompoundStmt(stmts: List[Stmt]) extends Stmt
case class SelectionStmt(expr: Expr, stmt1: Stmt, stmt2: Option[Stmt])
