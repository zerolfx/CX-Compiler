import model._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class CXParser extends StandardTokenParsers
  with PackratParsers {
  lexical.reserved ++= List("int", "bool", "if", "else", "while", "write", "read", "repeat", "until", "do",
    "exit", "for", "const", "function", "record")
  lexical.delimiters ++= List("++", "--", "+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!=", "=", "||", "&&", "!", ";", "(",
    ")", "{", "}", "/*", "*/", ",")

  lazy val type_specifier: PackratParser[Type] =
    "int" ^^ { _ => CXInt } |
    "real" ^^ { _ => CXReal } |
    "bool" ^^ { _ => CXBool }

  lazy val declaration_specifier: PackratParser[Type] =
    opt("const") ~ type_specifier ^^ {
      case None ~ tp => tp
      case Some(_) ~ tp => tp.toConst
    }

  lazy val declaration: Parser[DeclarationStmt] =
    declaration_specifier ~ repsep(init_declarator, ",") ^^ { case d ~ i => DeclarationStmt(d, i) }

  lazy val init_declarator: PackratParser[(Identifier, Option[Expr])] =
    (declarator <~ "=") ~ assignment_expression ^^ { case d ~ e => (d, Some(e))} |
    declarator ^^ { (_, None) }

  lazy val declarator: PackratParser[Identifier] =
    ident ^^ { SingleIdentifier } |
    ident ~ ("[" ~> constant_expression <~ "]").* ^^ { case i ~ e => ArrayIdentifier(i, e) }

  lazy val expression: PackratParser[Expr] = assignment_expression

  lazy val assignment_expression: PackratParser[Expr] =
    (ident <~ "=") ~ assignment_expression ^^ { case i ~ e => AssignExpr(SingleIdentifier(i), e)} |
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
    ("(" ~> type_specifier <~ ")") ~ cast_expression ^^ { case tp ~ e => CastExpr(tp, e) }

  lazy val unary_expression: PackratParser[Expr] =
    postfix_expression |
    ("++" | "--") ~ unary_expression ^^ { case op ~ e => UnaryOp(op, e) } |
    ("+" | "-" | "~" | "!") ~ cast_expression ^^ { case op ~ e => UnaryOp(op, e) }

  lazy val postfix_expression: PackratParser[Expr] =
    primary_expression |
    postfix_expression ~ ("++" | "--") ^^ { case e ~ op => UnaryOp("_" + op, e) }

  lazy val primary_expression: PackratParser[Expr] =
    numericLit ^^ { Num } |
    identifier |
    "(" ~> expression <~ ")"


  lazy val identifier: PackratParser[Identifier] =
    ident ~ rep("[" ~> expression <~ "]") ^^ { case i ~ e => ArrayIdentifier(i, e) } |
    ident ^^ { SingleIdentifier }

  lazy val compound_statement: PackratParser[CompoundStmt] =
    "{" ~> statement.* <~ "}" ^^ CompoundStmt

  lazy val statement: PackratParser[Stmt] =
    expression <~ ";" |
    declaration <~ ";" |
    compound_statement |
    "read" ~> identifier <~ ";" |
    "write" ~> expression <~ ";" ^^ { WriteStmt } |
    ";" ^^ { _ => EmptyStmt() }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }
}
