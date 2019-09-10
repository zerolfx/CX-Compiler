package model

import scala.util.parsing.input.Positional

class Expr extends Positional

case class Num(value: Int) extends Expr

case class BinaryOp(op: String, var left: Expr, right: Expr) extends Expr

case class Identifier(name: String) extends Expr

case class UnaryOp(op: String, expr: Expr) extends Expr

case class CastExpr(tp: String, expr: Expr) extends Expr

case class AssignExpr(id: Identifier, expr: Expr) extends Expr