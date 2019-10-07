package prooftree

import PL._

trait MathExpr {
  def toReducedString(reducedExprs: Map[CFWAE, String], reducedValues: Map[Value, String]): String = toString
}
case class Implication(env: Env, expr: CFWAE, value: Value) extends MathExpr {
  override def toString: String = s"${envToString(env)} ⊢ $expr ⇒ $value"
  override def toReducedString(reducedExprs: Map[CFWAE, String], reducedValues: Map[Value, String]): String =
    s"${envToReducedString(env, reducedValues)} ⊢ ${expr.toReducedString(reducedExprs)} ⇒ ${value.toReducedString(reducedValues)}"
}
case class Other(str: String) extends MathExpr {
  override def toString: String = str
}