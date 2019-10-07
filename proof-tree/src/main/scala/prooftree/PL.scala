package prooftree

object PL {
  trait CFWAE {
    def toReducedString(reduced: Map[CFWAE, String]): String = toString
  }
  case class Num(num: Int) extends CFWAE {
    override def toString: String = num.toString
  }
  case class Add(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{+ $left $right}"
    override def toReducedString(reduced: Map[CFWAE, String]): String = reduced.get(this) match {
      case Some(rs) => rs
      case None =>
        val leftStr = reduced.getOrElse(left, left.toReducedString(reduced))
        val rightStr = reduced.getOrElse(right, right.toReducedString(reduced))
        s"{+ $leftStr $rightStr}"
    }
  }
  case class Sub(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{- $left $right}"
    override def toReducedString(reduced: Map[CFWAE, String]): String = reduced.get(this) match {
      case Some(rs) => rs
      case None =>
        val leftStr = reduced.getOrElse(left, left.toReducedString(reduced))
        val rightStr = reduced.getOrElse(right, right.toReducedString(reduced))
        s"{- $leftStr $rightStr}"
    }
  }
  case class Mul(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{* $left $right}"
    override def toReducedString(reduced: Map[CFWAE, String]): String = reduced.get(this) match {
      case Some(rs) => rs
      case None =>
        val leftStr = reduced.getOrElse(left, left.toReducedString(reduced))
        val rightStr = reduced.getOrElse(right, right.toReducedString(reduced))
        s"{* $leftStr $rightStr}"
    }
  }
  case class With(name: String, value: CFWAE, body: CFWAE) extends CFWAE {
    override def toString: String = s"{with {$name $value} $body}"
    override def toReducedString(reduced: Map[CFWAE, String]): String = reduced.get(this) match {
      case Some(rs) => rs
      case None =>
        val valueStr = reduced.getOrElse(value, value.toReducedString(reduced))
        val bodyStr = reduced.getOrElse(body, body.toReducedString(reduced))
        s"{with {$name $valueStr} $bodyStr}"
    }
  }
  case class Id(name: String) extends CFWAE {
    override def toString: String = name
  }
  case class Fun(param: String, body: CFWAE) extends CFWAE {
    override def toString: String = s"{fun {$param} $body}"
    override def toReducedString(reduced: Map[CFWAE, String]): String = reduced.get(this) match {
      case Some(rs) => rs
      case None =>
        val bodyStr = reduced.getOrElse(body, body.toReducedString(reduced))
        s"{fun {$param} $bodyStr}"
    }
  }
  case class App(fun: CFWAE, arg: CFWAE) extends CFWAE {
    override def toString: String = s"{$fun $arg}"
    override def toReducedString(reduced: Map[CFWAE, String]): String = reduced.get(this) match {
      case Some(rs) => rs
      case None =>
        val funStr = reduced.getOrElse(fun, fun.toReducedString(reduced))
        val argStr = reduced.getOrElse(arg, arg.toReducedString(reduced))
        s"{$funStr $argStr}"
    }
  }
  case class If0(cond: CFWAE, tru: CFWAE, fls: CFWAE) extends CFWAE {
    override def toString: String = s"{if0 $cond $tru $fls}"
    override def toReducedString(reduced: Map[CFWAE, String]): String = reduced.get(this) match {
      case Some(rs) => rs
      case None =>
        val condStr = reduced.getOrElse(cond, cond.toReducedString(reduced))
        val truStr = reduced.getOrElse(tru, tru.toReducedString(reduced))
        val flsStr = reduced.getOrElse(fls, fls.toReducedString(reduced))
        s"{if0 $condStr $truStr $flsStr}"
    }
  }

  type Env = Map[String, Value]
  def envToString(env: Env): String = env.map({ case (n, v) => s"$n ↦ $v" }).mkString("[", ", ", "]")
  def envToReducedString(env: Env, reduced: Map[Value, String]): String =
    env.map({ case (n, v) => s"$n ↦ ${v.toReducedString(reduced)}" }).mkString("[", ", ", "]")

  trait Value {
    def toReducedString(reduced: Map[Value, String]): String = toString
  }
  case class NumVal(num: Int) extends Value {
    override def toString: String = num.toString
  }
  case class FunVal(param: String, body: CFWAE, funEnv: Env) extends Value {
    override def toString: String = s"⟨λ$param.$body, ${envToString(funEnv)}⟩"
  }
}