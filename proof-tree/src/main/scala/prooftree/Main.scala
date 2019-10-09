package prooftree

import java.io._

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 2) {
      println(s"Reading ${args(0)}...")
      val source = scala.io.Source.fromFile(args(0))
      val str = source.mkString
      source.close()

      println(s"Writing the proof tree to ${args(1)}...")
      val pw = new PrintWriter(args(1))
      pw.print(ProofTreeDrawer.run(str))
      pw.close()

      println("Done!")
    }
    else {
      println("Usage: sbt \"run input.txt output.txt\"")
    }
  }
}