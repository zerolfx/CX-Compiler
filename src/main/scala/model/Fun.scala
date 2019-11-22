package model

case class Fun(name: String, returnType: Type, argumentList: List[(Identifier, Type)], stmts: CompoundStmt) extends Node {
  override def gen(implicit env: Env): String = {
    env.symbolTable.allocated = 5
    env.symbolTable.maxAllocated = 5
    env.symbolTable.openEnv()
    argumentList.foreach(x => env.symbolTable.registerIdentifier(x._1, x._2))
    val res = stmts.gen
    env.symbolTable.closeEnv()
    Ins.label("fun" + name) + Ins.ssp(env.symbolTable.maxAllocated) + res + Ins.retp
  }
}
