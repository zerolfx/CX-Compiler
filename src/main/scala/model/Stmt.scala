package model

abstract class Stmt extends Node


case class WriteStmt(expr: Expr) extends Stmt {
  override def gen(implicit env: Env): String = expr.gen(env) + Ins.out(expr.tp.code) +
    Ins.ldc(Ins.c, "'\\n'") + Ins.out(Ins.c)
}
case class ReadStmt(identifier: Identifier) extends Stmt {
  override def gen(implicit env: Env): String = Ins.in(identifier.tp.code)
}

case class CompoundStmt(stmts: List[Stmt]) extends Stmt {
  override def gen(implicit env: Env): String = {
    env.symbolTable.openEnv()
    val res = stmts.map(_.gen).mkString
    env.symbolTable.closeEnv()
    res
  }
}

case class DeclarationStmt(tp: Type, as: List[(Identifier, Option[Expr])]) extends Stmt {
  override def gen(implicit env: Env): String = {
    as.map {
      case (identifier: ArrayIdentifier, expr) =>
        assert(expr.isEmpty)
        env.symbolTable.registerIdentifier(identifier, CXArray(tp, identifier.sub.map {
          case Num(numberLit) => numberLit.toInt
        }))
        val (arrTp, addr) = env.symbolTable.getIdentifier(identifier).get
        (0 until arrTp.getSize).map(i =>
          Ins.ldc(tp.code, tp.default) + Ins.str(tp.code, 0, addr + i)).mkString
      case (identifier, expr) =>
        env.symbolTable.registerIdentifier(identifier, tp)
        expr.fold(Ins.ldc(tp.code, tp.default))(_.gen) + Ins.str(tp.code, 0, env.symbolTable.getIdentifier(identifier).get._2)
    }.mkString
  }
}

case object EmptyStmt extends Stmt {
  override def gen(implicit env: Env): String = ""
}

case object ExitStmt extends Stmt {
  override def gen(implicit env: Env): String = Ins.hlt
}

case class ExprStmt(expr: Expr) extends Stmt {
  override def gen(implicit env: Env): String = expr.gen + Ins.pop
}

case class IfStmt(expr: Expr, s1: Stmt, s2: Stmt) extends Stmt {
  override def gen(implicit env: Env): String = {
    val elseLabel = Ins.createLabel
    val endLabel = Ins.createLabel
    expr.gen +
      (if (!expr.tp.isInstanceOf[CXBool]) Ins.conv(expr.tp.code, Ins.b) else "") +
      Ins.fjp(elseLabel) + s1.gen + Ins.ujp(endLabel) +
      Ins.label(elseLabel) + s2.gen + Ins.label(endLabel)
  }
}

// for (s1 s2 s3) s4
case class ForStmt(s1: Stmt, s2: Expr, s3: Stmt, s4: Stmt) extends Stmt {
  override def gen(implicit env: Env): String = {
    val startLabel = Ins.createLabel
    val endLabel = Ins.createLabel
    env.symbolTable.openEnv()
    val res = s1.gen +
      Ins.label(startLabel) + s4.gen + s3.gen + s2.gen + Ins.fjp(endLabel) +
      Ins.ujp(startLabel) + Ins.label(endLabel)
    env.symbolTable.closeEnv()
    res
  }
}

case class ReturnStmt(ret: Option[Expr]) extends Stmt {
  override def gen(implicit env: Env): String = ret match {
    case Some(e) => e.gen + Ins.str(e.tp.code, 0, 0) + Ins.retf
    case None => Ins.retp
  }
}