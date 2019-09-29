import model.{BinaryOp, CompoundStmt, DeclarationStmt, EmptyStmt, Expr, Num, ReadStmt, Scope, SingleIdentifier, Stmt, UnaryOp, WriteStmt}



class CXCodeGenerator {
  type PCode = (String, Int, Int)
  def gen_statement(stmt: Stmt)(implicit scope: Scope): List[PCode] = stmt match {
    case EmptyStmt() => Nil
    case CompoundStmt(stmts) =>
      stmts.flatMap(gen_statement(_)(new Scope(Some(scope))))
    case DeclarationStmt(tp, as) =>
      ("lit", 0, as.length) :: as.flatMap {
        case (i: SingleIdentifier, init_val) =>
          assert(!scope.vars.contains(i.name))
          scope.vars ++ i.name -> (tp, scope.vars.size)
          init_val.fold(Nil: List[PCode])(gen_expr)
      }
    case WriteStmt(expr) =>
      gen_expr(expr) :+ ("wri", 0, 0)
  }
  def gen_expr(expr: Expr)(implicit scope: Scope): List[PCode] = expr match {
    case Num(value) => List(("lit", 0, value))
    case BinaryOp("+", l, r) => gen_expr(l) ++ gen_expr(r) :+ ("opr", 0, 2)
    case BinaryOp("-", l, r) => gen_expr(l) ++ gen_expr(r) :+ ("opr", 0, 3)
    case BinaryOp("*", l, r) => gen_expr(l) ++ gen_expr(r) :+ ("opr", 0, 4)
    case BinaryOp("/", l, r) => gen_expr(l) ++ gen_expr(r) :+ ("opr", 0, 5)
  }
}
