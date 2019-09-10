import jdk.nashorn.internal.ir.Block
import model._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class CXParser extends StandardTokenParsers with PackratParsers {
  lexical.reserved ++= List("int", "bool", "if", "else", "while", "write", "read", "repeat", "until", "do",
    "exit", "for", "const", "function", "record")
  lexical.delimiters ++= List("++", "--", "+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!=", "=", "||", "&&", "!", ";", "(",
    ")", "{", "}", "/*", "*/")

  def type_name: Parser[String] = "int" | "bool"

  lazy val expression: PackratParser[Expr] = assignment_expression

  lazy val assignment_expression: PackratParser[Expr] =
    (ident <~ "=") ~ assignment_expression ^^ { case i ~ e => AssignExpr(Identifier(i), e)} |
    constant_expression

  def build_binary_op_expr(old_expr: PackratParser[Expr], op: Parser[String]): PackratParser[Expr] = {
    lazy val new_expr: PackratParser[Expr] =
      new_expr ~ op ~ old_expr ^^ { case a ~ op ~ b => BinaryOp(op, a, b) } |
        old_expr
    new_expr
  }

  lazy val constant_expression: PackratParser[Expr] = List[Parser[String]](
    "*" | "/" | "%",
    "+" | "-",
    "<" | ">" | "<=" | ">=",
    "==" | "!=",
    "&&",
    "||").foldLeft(cast_expression)(build_binary_op_expr)

  lazy val cast_expression: PackratParser[Expr] =
    unary_expression |
    ("(" ~> type_name <~ ")") ~ cast_expression ^^ { case tp ~ e => CastExpr(tp, e) }

  lazy val unary_expression: PackratParser[Expr] =
    postfix_expression |
    ("++" | "--") ~ unary_expression ^^ { case op ~ e => UnaryOp(op, e) } |
    ("+" | "-" | "~" | "!") ~ cast_expression ^^ { case op ~ e => UnaryOp(op, e) }

  lazy val postfix_expression: PackratParser[Expr] =
    primary_expression |
    postfix_expression ~ ("++" | "--") ^^ { case e ~ op => UnaryOp("_" + op, e) } |
    postfix_expression ~ ("[" ~> expression <~ "]") ^^ { case a ~ b => BinaryOp("[]", a, b) }

  lazy val primary_expression: PackratParser[Expr] =
    numericLit ^^ { a => Num(a.toInt) } |
    ident ^^ { Identifier } |
    "(" ~> expression <~ ")"

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }
}
