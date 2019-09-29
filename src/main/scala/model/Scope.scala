package model

import collection.mutable.HashMap
import scala.collection.mutable

class Scope(parent: Option[Scope]) {
  var vars = mutable.HashMap.empty[String, Int]
}
