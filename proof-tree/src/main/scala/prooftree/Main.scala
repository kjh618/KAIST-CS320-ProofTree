package prooftree

object Main extends App {
  println()
  println(ProofTreeDrawer.run("{+ 4 {- 2 1}}"))
  println()
  println(ProofTreeDrawer.run("{with {x 1} {+ x 2}}"))
  println()
  println(ProofTreeDrawer.run("{fun {x} x}"))
  println()
}