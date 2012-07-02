package karolmstasiak.screw.test
import karolmstasiak.screw._

object RunTests {

  def main(args: Array[String]): Unit = {
    val arr = new Array[Byte](0)
    val size = 10
    (0 until 100) foreach { i =>
      if(i%10==0) println()
      print(" "+i+"% ")
      (0 until size) foreach { _=>
        new Test(arr).test(Test.generate(1500), false)
      }
    }
    println(" 100%")
  }

}