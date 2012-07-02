package karolmstasiak.screw
import scala.io.Source
import java.io.File
import java.io.PrintStream
import java.io.FileInputStream
import karolmstasiak.screw.test._
import karolmstasiak.screw.optimize._
import karolmstasiak.screw.emitter._
import karolmstasiak.screw._

object Main {

  def printHelp(){
    //TODO
    System.exit(0)
  }
  def main(args: Array[String]): Unit = {
    var filename = "/home/karol/notinportland/bfscala/LostKng.b"
    var optimizeLevel = 6
    var littleEndian: Option[Boolean] = None
    var preinitializationLevel = 4
    var emitter: Emitter = CEmitter
    var outputStream = new PrintStream(new File("/home/karol/notinportland/bfscala/LostKng3.c"))
      //System.out
    var trimUseless = true
    var inputTestFile = "/home/karol/notinportland/bfscala/testInput.txt"
    var tapeSize = 30000
    var allocateTapeOnHeap = false
    
    var testOptimization = false
    
    var i=0
    while(i<args.size){
      args(i) match{
        case "-O0" =>
          optimizeLevel = 0
          preinitializationLevel = 0
          trimUseless = false
        case "-O" | "-O1" =>
          optimizeLevel = 1
          preinitializationLevel = 1
          trimUseless = true
        case "-O2" =>
          optimizeLevel = 3
          preinitializationLevel = 2
          trimUseless = true
        case "-O3" =>
          optimizeLevel = 5
          preinitializationLevel = 3
          trimUseless = true
        case "-Os" =>
          optimizeLevel = 4
          preinitializationLevel = 1
          trimUseless = true
        case "--target=c" =>
          emitter = CEmitter
        case "--target=java" =>
          emitter = JavaEmitter
        case "--target=python" =>
          emitter = PythonEmitter
        case "-OT" =>
          testOptimization = true
        case "-TI" =>
          i += 1
          if(i < args.length) {
            inputTestFile = args(i)
          }
        case "-o" =>
          i += 1
          if(i < args.length) {
            outputStream = new PrintStream(new File(args(i)))
          }
        case "-ts" =>
          i += 1
          if(i < args.length) {
            tapeSize = args(i).toInt
          }
        case "--" =>
          i += 1
          if(i < args.length) {
            filename = args(i)
          }
        case _ =>
          filename = args(i)
      }
      i += 1
    }
    
    val code = Parser.parseBf(Source.fromFile(filename).mkString)
    if(testOptimization){
      val is = new FileInputStream(inputTestFile)
      val arr = new Array[Byte](1000000)
      is.read(arr)
      new Test(arr).test(code, false)
      is.close()
    }
    
    emitter.setHeapTape(allocateTapeOnHeap)
    emitter.emit(tapeSize, new Optimizer(
        optimizeLevel,littleEndian,preinitializationLevel,trimUseless
    ).optimize(code), outputStream)
    if(outputStream ne System.out) outputStream.close()
  }

}