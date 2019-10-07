package prooftree

import java.io._

object Main extends App {
  println(ProofTreeDrawer.run("{+ 1 {- 2 3}}"))
  println()
  println(ProofTreeDrawer.run("{fun {x} x}"))
  println()
  println(ProofTreeDrawer.run("{with {add1 {fun {x} {+ x 1}}} {add1 2}}"))

//  val pw = new PrintWriter("out.txt")
//
//  pw.println(ProofTreeDrawer.run(
//    """{with {fac
//      |  {with {facX {fun {facY}
//      |    {with {fac {fun {x} {{facY facY} x}}}
//      |
//      |    {fun {n} {if0 n 1 {* n {fac {- n 1}}}  }}}}}
//      |
//      |  {facX facX}  }}
//      |
//      |{fac 1}  }
//      |""".stripMargin))
//  pw.println()
//  pw.println(ProofTreeDrawer.run(
//    """{with {fac
//      |  {with {facX {fun {facY}
//      |    {with {fac {fun {x} {{facY facY} x}}}
//      |
//      |    {fun {n} {if0 n 1 {* n {fac {- n 1}}}  }}}}}
//      |
//      |  {facX facX}  }}
//      |
//      |{fac 2}  }
//      |""".stripMargin))
//
//  pw.close()

  println("Done!")
}