package prooftree

import prooftree.Env._

// Value in CFWAE
trait Value {
  def toReducedString(numNotReduce: Int): String
}

case class NumVal(num: Int) extends Value {
  override def toString: String = num.toString

  override def toReducedString(numNotReduce: Int): String = Reduced.values.get(this) match {
    case Some(rs) => rs
    case None => num.toString
  }
}

case class FunVal(param: String, body: Expr, funEnv: Env) extends Value {
  override def toString: String = s"⟨λ$param.$body, ${envToString(funEnv)}⟩"

  override def toReducedString(numNotReduce: Int): String = Reduced.values.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val bodyStr = body.toReducedString(numNotReduce - 1)
      val funEnvStr = envToReducedString(funEnv, numNotReduce - 1)
      val res = s"⟨λ$param.$bodyStr, $funEnvStr⟩"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"v%0${Reduced.numDigits}d".format(Reduced.values.size)
        Reduced.values += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}