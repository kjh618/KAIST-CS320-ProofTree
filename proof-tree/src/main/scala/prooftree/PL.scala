package prooftree

object PL {
  trait CFWAE
  case class Num(num: Int) extends CFWAE {
    override def toString: String = num.toString
  }
  case class Add(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{+ $left $right}"
  }
  case class Sub(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{- $left $right}"
  }
  case class Mul(left: CFWAE, right: CFWAE) extends CFWAE {
    override def toString: String = s"{* $left $right}"
  }
  case class With(name: String, value: CFWAE, body: CFWAE) extends CFWAE {
    override def toString: String = s"{with {$name $value} $body}"
  }
  case class Id(name: String) extends CFWAE {
    override def toString: String = name
  }
  case class Fun(param: String, body: CFWAE) extends CFWAE {
    override def toString: String = s"{fun {$param} $body}"
  }
  case class App(fun: CFWAE, arg: CFWAE) extends CFWAE {
    override def toString: String = s"{$fun $arg}"
  }
  case class If0(cond: CFWAE, tru: CFWAE, fls: CFWAE) extends CFWAE {
    override def toString: String = s"{if0 $cond $tru $fls}"
  }

  type Env = Map[String, Value]
  def envToString(env: Env): String = env.map({ case (n, v) => s"$n ↦ $v" }).mkString("[", ", ", "]")

  trait Value
  case class NumVal(num: Int) extends Value {
    override def toString: String = num.toString
  }
  case class FunVal(param: String, body: CFWAE, funEnv: Env) extends Value {
    override def toString: String = s"⟨λ$param.$body, ${envToString(funEnv)}⟩"
  }
}