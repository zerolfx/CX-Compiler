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

  def build_binary_op(x: Expr ~ String ~ Expr): BinaryOp =
    x match { case a ~ op ~ b => BinaryOp(op, a, b) }

  lazy val expression: PackratParser[Expr] = assignment_expression

  lazy val assignment_expression: PackratParser[Expr] =
    (ident <~ "=") ~ assignment_expression ^^ { case i ~ e => AssignExpr(Identifier(i), e)} |
    logical_or_expression

//  lazy val constant_expression: PackratParser[Expr] = logical_or_expression

  lazy val logical_or_expression: PackratParser[Expr] =
    logical_or_expression ~ "||" ~ logical_and_expression ^^ build_binary_op |
    logical_and_expression

  lazy val logical_and_expression: PackratParser[Expr] =
    logical_and_expression ~ "&&" ~ equality_expression ^^ build_binary_op |
    equality_expression

  lazy val equality_expression: PackratParser[Expr] =
    equality_expression ~ ("==" | "!=") ~ relational_expression ^^ build_binary_op |
    relational_expression

  lazy val relational_expression: PackratParser[Expr] =
    relational_expression ~ ("<" | ">" | "<=" | ">=") ~ additive_expression ^^ build_binary_op |
    additive_expression

  lazy val additive_expression: PackratParser[Expr] =
    additive_expression ~ ("+" | "-") ~ multiplicative_expression ^^ build_binary_op |
    multiplicative_expression

  lazy val multiplicative_expression: PackratParser[Expr] =
    multiplicative_expression ~ ("*" | "/" | "%") ~ cast_expression ^^ build_binary_op |
    cast_expression

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

//  def stmt: Parser[Stmt] =
//    "read" ~> ident <~ ";" ^^ { ReadStmt } |
//    "write" ~> expr <~ ";" ^^ { WriteStmt } |
//    ident ~> "=" ~ expr <~ ";" ^^ { case i ~ e => AssignStmt(i, e) } |
//    ("int" | "bool") ~ ident <~ ";" ^^ { case t ~ i => DeclStmt(t, i) }
//
//  lazy val  stmts: PackratParser[List[Stmt]] =
//    rep(stmts) ^^ { _.flatten }
//    "{" ~>  rep(stmt) <~ "}" ^^ identity |
//    stmt ^^ { List(_) }
//
//  val program: Parser[List[Stmt]] = stmts

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }
}
