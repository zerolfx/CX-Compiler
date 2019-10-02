package model

case class Program(funs: List[Fun], stmts: CompoundStmt) {
  def gen: String = {
    val funTable = new FunTable
    funs.foreach(funTable.registerFunction)
    val r1 = funs.map(_.gen(Env(funTable, new SymbolTable))).mkString

    val env = Env(funTable, new SymbolTable)
    val res = stmts.gen(env)
    Ins.ujp("main") + r1 + Ins.label("main") + Ins.ssp(env.symbolTable.maxAllocated) + res
  }
}