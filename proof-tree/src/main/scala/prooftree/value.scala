package prooftree

import prooftree.Env._

// Value in CFWAE
trait Value {
  def toReducedString: String
}

case class NumVal(num: Int) extends Value {
  override def toString: String = num.toString

  override def toReducedString: String = Reduced.values.get(this) match {
    case Some(rs) => rs
    case None => num.toString
  }
}

case class FunVal(param: String, body: Expr, funEnv: Env) extends Value {
  override def toString: String = s"⟨λ$param.$body, ${envToString(funEnv)}⟩"

  override def toReducedString: String = Reduced.values.get(this) match {
    case Some(rs) => rs
    case None =>
      val bodyStr = Reduced.exprs.getOrElse(body, body.toReducedString)
      val funEnvStr = envToReducedString(funEnv)
      s"⟨λ$param.$bodyStr, $funEnvStr⟩"
  }
}