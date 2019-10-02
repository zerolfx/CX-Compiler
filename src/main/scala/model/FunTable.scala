package model

import scala.collection.mutable

class FunTable {
  val funs = mutable.HashMap.empty[String, (Type, List[(Identifier, Type)])]

  def registerFunction(fun: Fun): Unit = {
    if (funs.contains(fun.name)) throw new Exception(f"${fun.name} is used")
    funs += fun.name -> (fun.returnType, fun.argumentList)
  }

  def getFunction(name: String): Option[(Type, List[(Identifier, Type)])] = funs.get(name)
}


