package prooftree

// Expression in CFWAE
trait Expr {
  def toReducedString(numNotReduce: Int): String
}

case class Num(num: Int) extends Expr {
  override def toString: String = num.toString

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) => rs
    case None => num.toString
  }
}

case class Add(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{+ $left $right}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val leftStr = left.toReducedString(numNotReduce - 1)
      val rightStr = right.toReducedString(numNotReduce - 1)
      val res = s"{+ $leftStr $rightStr}"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"e%0${Reduced.numDigits}d".format(Reduced.exprs.size)
        Reduced.exprs += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}

case class Sub(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{- $left $right}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val leftStr = left.toReducedString(numNotReduce - 1)
      val rightStr = right.toReducedString(numNotReduce - 1)
      val res = s"{- $leftStr $rightStr}"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"e%0${Reduced.numDigits}d".format(Reduced.exprs.size)
        Reduced.exprs += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}

case class Mul(left: Expr, right: Expr) extends Expr {
  override def toString: String = s"{* $left $right}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val leftStr = left.toReducedString(numNotReduce - 1)
      val rightStr = right.toReducedString(numNotReduce - 1)
      val res = s"{* $leftStr $rightStr}"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"e%0${Reduced.numDigits}d".format(Reduced.exprs.size)
        Reduced.exprs += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}

case class With(name: String, value: Expr, body: Expr) extends Expr {
  override def toString: String = s"{with {$name $value} $body}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val valueStr = value.toReducedString(numNotReduce - 1)
      val bodyStr = body.toReducedString(numNotReduce - 1)
      val res = s"{with {$name $valueStr} $bodyStr}"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"e%0${Reduced.numDigits}d".format(Reduced.exprs.size)
        Reduced.exprs += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}

case class Id(name: String) extends Expr {
  override def toString: String = name

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case None => name
  }
}

case class Fun(param: String, body: Expr) extends Expr {
  override def toString: String = s"{fun {$param} $body}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val bodyStr = body.toReducedString(numNotReduce - 1)
      val res = s"{fun {$param} $bodyStr}"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"e%0${Reduced.numDigits}d".format(Reduced.exprs.size)
        Reduced.exprs += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}

case class App(fun: Expr, arg: Expr) extends Expr {
  override def toString: String = s"{$fun $arg}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val funStr = fun.toReducedString(numNotReduce - 1)
      val argStr = arg.toReducedString(numNotReduce - 1)
      val res = s"{$funStr $argStr}"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"e%0${Reduced.numDigits}d".format(Reduced.exprs.size)
        Reduced.exprs += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}

case class If0(cond: Expr, tru: Expr, fls: Expr) extends Expr {
  override def toString: String = s"{if0 $cond $tru $fls}"

  override def toReducedString(numNotReduce: Int): String = Reduced.exprs.get(this) match {
    case Some(rs) if numNotReduce == 0 => rs
    case opt =>
      val condStr = cond.toReducedString(numNotReduce - 1)
      val truStr = tru.toReducedString(numNotReduce - 1)
      val flsStr = fls.toReducedString(numNotReduce - 1)
      val res = s"{if0 $condStr $truStr $flsStr}"
      if (opt.isEmpty && res.length > Reduced.notReduceLength && numNotReduce == 0) {
        val rs = s"e%0${Reduced.numDigits}d".format(Reduced.exprs.size)
        Reduced.exprs += (this -> rs)
        rs
      }
      else {
        res
      }
  }
}