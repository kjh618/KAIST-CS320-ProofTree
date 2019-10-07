package prooftree

import scala.sys.error
import prooftree.PL._

object Interpreter {
  def interp(expr: CFWAE, env: Env): Value = {
    type BinOp = (Int, Int) => Int

    def applyBinOp(op: BinOp, left: Value, right: Value): Value = (left, right) match {
      case (NumVal(n1), NumVal(n2)) => NumVal(op(n1, n2))
      case (v1, v2) => error(s"not both numbers: $v1, $v2")
    }

    expr match {
      case Num(n) => NumVal(n)
      case Add(l, r) => applyBinOp(_ + _, interp(l, env), interp(r, env))
      case Sub(l, r) => applyBinOp(_ - _, interp(l, env), interp(r, env))
      case Mul(l, r) => applyBinOp(_ * _, interp(l, env), interp(r, env))
      case With(name, value, body) => interp(body, env + (name -> interp(value, env)))
      case Id(name) => env.getOrElse(name, error(s"free identifier: $name"))
      case Fun(param, body) => FunVal(param, body, env)
      case App(fun, arg) => interp(fun, env) match {
        case FunVal(param, body, funEnv) => interp(body, funEnv + (param -> interp(arg, env)))
        case v => error(s"not a function: $v")
      }
      case If0(cond, tru, fls) => interp(cond, env) match {
        case NumVal(0) => interp(tru, env)
        case _ => interp(fls, env)
      }
    }
  }

  def run(str: String): String = interp(PLParser(str), Map()).toString
}