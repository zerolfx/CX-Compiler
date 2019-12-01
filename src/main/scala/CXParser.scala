import model._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class CXStdLexer extends StdLexical {
  // to support float number
  override def token: Parser[Token] =
    rep(digit) ~ '.' ~ rep(digit) ^^ { case a ~ b ~ c => NumericLit(a.mkString + b + c.mkString) } |
    super.token
}

class CXParser extends StandardTokenParsers
  with PackratParsers {
  override val lexical = new CXStdLexer

  lexical.reserved ++= List(
    "int", "bool", "real", "const", // type
    "if", "else", // selection
    "while", "repeat", "until", "do", "for", // loop
    "write", "read", // builtins
    "exit",  "function", "return",
    "true", "false",
    "and", "or", "not", "xor"
  )
  lexical.delimiters ++= List(
    "++", "--",
    "+", "-", "*", "/", "%",
    "<", "<=", ">", ">=", "==", "!=", "=",
    "||", "&&", "!", "^",
    ";",
    "(", ")", "{", "}", "[", "]",
    "/*", "*/", ",",
  )

  lazy val type_specifier: Parser[Type] =
    "int" ^^ { _ => CXInt() } |
    "real" ^^ { _ => CXReal() } |
    "bool" ^^ { _ => CXBool() } |
    "void" ^^^ CXVoid

  lazy val declaration_specifier: PackratParser[Type] =
    opt("const") ~ type_specifier ^^ {
      case None ~ tp => tp
      case Some(_) ~ tp => tp.toConst
    }

  lazy val declaration: Parser[DeclarationStmt] =
    declaration_specifier ~ rep1sep(init_declarator, ",") ^^ { case d ~ i => DeclarationStmt(d, i) }

  lazy val init_declarator: PackratParser[(Identifier, Option[Expr])] =
    (declarator <~ "=") ~ assignment_expression ^^ { case d ~ e => (d, Some(e))} |
    declarator ^^ { (_, None) }

  lazy val declarator: PackratParser[Identifier] = identifier

  lazy val expression: PackratParser[Expr] = assignment_expression

  lazy val assignment_expression: PackratParser[Expr] =
    (identifier <~ "=") ~ assignment_expression ^^ { case i ~ e => AssignExpr(i, e)} |
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
    "&&" | "and",
    "||" | "or" | "xor").foldLeft(cast_expression)(build_binary_op_expr)

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
    ident ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ { case id ~ args => FunctionCallExpr(id, args) } |
    numericLit ^^ Num |
    ("true" | "false") ^^ { s => Num(s.substring(0, 1)) } |
    identifier |
    "(" ~> expression <~ ")"


  lazy val identifier: PackratParser[Identifier] =
    ident ~ rep1("[" ~> expression <~ "]") ^^ { case i ~ e => ArrayIdentifier(i, e) } |
    ident ^^ SingleIdentifier

  lazy val compound_statement: PackratParser[CompoundStmt] =
    "{" ~> statement.* <~ "}" ^^ CompoundStmt

  lazy val statement: PackratParser[Stmt] =
    expression <~ ";" ^^ ExprStmt |
    declaration <~ ";" |
    compound_statement |
    "read" ~> identifier <~ ";" ^^ ReadStmt |
    "write" ~> expression <~ ";" ^^ WriteStmt |
    ";" ^^^  EmptyStmt |
    "if" ~> ("(" ~> expression <~ ")") ~ statement ~ opt("else" ~> statement) ^^ {
      case e ~ s1 ~ s2 => IfStmt(e, s1, s2.getOrElse(EmptyStmt))
    } |
    "for" ~> ("(" ~> (statement ~ (expression <~ ";") ~ expression) <~ ")") ~ statement ^^ {
      case s1 ~ s2 ~ s3 ~ s4 => ForStmt(s1, s2, ExprStmt(s3), s4)
    } |
    "while" ~> ("(" ~> expression <~ ")") ~ statement ^^ { case e ~ s => ForStmt(EmptyStmt, e, EmptyStmt, s); } |
    ("do" ~> statement <~ "while") ~ ("(" ~> expression <~ ")") <~ ";" ^^ {case s ~ e => ForStmt(s, e, EmptyStmt, s) } | // TODO need optimize
    "return" ~> opt(expression) <~ ";" ^^ ReturnStmt |
    "exit" ^^^ ExitStmt

  lazy val program: PackratParser[Program] = rep(function) ~ compound_statement ^^ {
    case f ~ p => Program(f, p)
  }

  lazy val function: PackratParser[Fun] =
    declaration_specifier ~ ident ~ ("(" ~> repsep(declaration_specifier ~ identifier, ",") <~ ")") ~ compound_statement ^^ {
      case tp ~ id ~ args ~ stmts => Fun(id, tp, args.map { case tp ~ id => (id, tp) }, stmts)
    }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }
}
