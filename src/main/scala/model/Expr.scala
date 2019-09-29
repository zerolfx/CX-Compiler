package model

import scala.util.parsing.input.Positional

class Expr extends Stmt

case class Num(value: Int) extends Expr

case class BinaryOp(op: String, var left: Expr, right: Expr) extends Expr

class Identifier extends Expr

case class ArrayIdentifier(name: String, sub: List[Expr]) extends Identifier
case class SingleIdentifier(name: String) extends Identifier

case class UnaryOp(op: String, expr: Expr) extends Expr

case class CastExpr(tp: Type, expr: Expr) extends Expr

case class AssignExpr(id: Identifier, expr: Expr) extends Expr