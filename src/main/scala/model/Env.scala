package model

import collection.mutable.{HashMap, ListBuffer}
import scala.collection.mutable

class Env(parentEnv: Option[Env]) {
  val envs: ListBuffer[Env] = collection.mutable.ListBuffer.empty[Env]

  def openEnv: Env = {
    envs.addOne(new Env(Some(this)))
    envs.last
  }

//  def closeEnv: E
}