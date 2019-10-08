package prooftree

import java.io._

object Main {
  def main(args: Array[String]): Unit = {
    val pw = new PrintWriter("out.txt")

    pw.println(ProofTreeDrawer.run(
      """{with {fac
        |  {with {facX {fun {facY}
        |    {with {fac {fun {x} {{facY facY} x}}}
        |
        |    {fun {n} {if0 n 1 {* n {fac {- n 1}}}  }}}}}
        |
        |  {facX facX}  }}
        |
        |{fac 1}  }
        |""".stripMargin))
    pw.println()
    
    pw.println(ProofTreeDrawer.run(
      """{with {fac
        |  {with {facX {fun {facY}
        |    {with {fac {fun {x} {{facY facY} x}}}
        |
        |    {fun {n} {if0 n 1 {* n {fac {- n 1}}}  }}}}}
        |
        |  {facX facX}  }}
        |
        |{fac 2}  }
        |""".stripMargin))

    pw.close()

    println("Done!")
  }
}