package prooftree

import scala.collection.mutable

object Reduced {
  val notReduceLength = 20
  val numNotReduce = 1
  val numDigits = 1

  val exprs: mutable.Map[Expr, String] = mutable.Map()
  val values: mutable.Map[Value, String] = mutable.Map()

  def initialize(): Unit = {
    exprs.clear()
    values.clear()
  }
}