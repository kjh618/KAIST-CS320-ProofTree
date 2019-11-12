package prooftree

object Main {
  def main(args: Array[String]): Unit = {
    val str = "{+ 1 2}"
    println(ProofTreeDrawer.run(str))
  }
}