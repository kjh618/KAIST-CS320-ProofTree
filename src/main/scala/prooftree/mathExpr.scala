package prooftree

import prooftree.Env._

trait MathExpr {
  def toReducedString(): String
}

case class Implication(env: Env, expr: Expr, value: Value) extends MathExpr {
  override def toString: String = s"${envToString(env)} ⊢ $expr ⇒ $value"

  override def toReducedString(): String = {
    val exprStr = expr.toReducedString(Reduced.numNotReduce)
    val valueStr = value.toReducedString(0)
    val envStr = envToReducedString(env, 0)
    s"$envStr ⊢ $exprStr ⇒ $valueStr"
  }
}

case class Membership(name: String, env: Env) extends MathExpr {
  override def toString: String = s"$name ∈ ${envDomainToString(env)}"

  override def toReducedString(): String = s"$name ∈ ${envDomainToString(env)}"
}

case class NotEqualTo0(value: Value) extends MathExpr {
  override def toString: String = s"$value ≠ 0"

  override def toReducedString(): String = s"${value.toReducedString(0)} ≠ 0"
}