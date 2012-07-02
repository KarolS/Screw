package karolmstasiak.screw.test
import karolmstasiak.screw._
import karolmstasiak.screw.optimize._
import java.io.ByteArrayOutputStream
import java.util.Arrays.{equals => AEQ}
import scala.math.sqrt
import scala.util.Random
import java.io.ByteArrayInputStream


object Test {
  def generate(length:Int=100):String = {
    if(length<=0) return ""
    if(length>3 && Random.nextDouble<0.1) return "[-]" + generate(length-3)
    if(length>10 && Random.nextDouble<0.1) {
      val i = 4 max Random.nextInt(sqrt(length*sqrt(length)+66).toInt+1)
      return "[" + generate(i) + "]" + generate(length-i-2)
    }
    if(length>10) return singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      singleTokens(Random.nextInt(5)).toString + 
      generate(length-10)
    return singleTokens(Random.nextInt(5)).toString + generate(length-10)
  }
  val singleTokens = ".+-<>"

}
class Test(input: Array[Byte]){
  val optimizer = new Optimizer(3, None, -1, false)
  def dumbSimplify(program:String):String={
    val p2 = program.replaceAll("+-","").replaceAll("-+","").replaceAll("<>","").replaceAll("><","")
    if(p2==program) program else dumbSimplify(p2)
  }
  def simplifyBadCase(programL:String):String={
    val program = dumbSimplify(programL)
    for(i<-0 until program.length){
      if (program(i)!='[' && program(i)!=']'){
        val shorter = program.substring(0,i) + program.substring(i,program.size)
        if(test(shorter,true)) return simplifyBadCase(shorter)//A nie zanegowane?
      }
    }
    program
  }
  def testOne(program: String){
    testOne(program,Parser.parseBf(program))
  }
  def testOne(source:String, program:List[Expr]){
      if(!test(program, false)){
        System.err.println("\nProgram optimized incorrectly:")
        System.err.println(program)
        val program2 = simplifyBadCase(source)
        System.err.println("\nThe case, simplified:")
        System.err.println(program2)
      }
      else{
        System.err.print("ok...")
      }
    
  }
  
  def testBundle(count:Int){
    for(_ <- 0 until count){
      val program = Test.generate()
      testOne(program, Parser.parseBf(program))
    }
  }

  def test(program:String, quiet:Boolean):Boolean = test(Parser.parseBf(program),quiet)
  def test(unoptimized:List[Expr], quiet:Boolean): Boolean={
    try{
      val linearized = optimizer.linearize(unoptimized)
      val optimized = optimizer.optimize(unoptimized)
      val (a1,o1) = pull(unoptimized)
      if(Runner.ticks<=0) {
        if(!quiet)System.err.print("timeout...") 
        return true
      }
      if(!quiet){
        val (a2,o2) = pull(linearized)
        if(Runner.ticks<=0) {
          System.err.print("timeout...") 
          return true
        }
        if(!AEQ(a1,a2)) {
          System.err.print("\nAfter linearization: Different memory contents. ")
          return false
        }
        if(!AEQ(o1,o2)) {
          System.err.print("\nAfter linearization: Different outputs. ")
          return false
        }
      }
      val (a3,o3) = pull(optimized)
      if(Runner.ticks<=0) {
        if(!quiet)System.err.print("timeout...") 
        return true
      }
      if(!AEQ(a1,a3)) {
        if(!quiet)System.err.print("\nAfter optimization: Different memory contents. ")
        return false
      }
      if(!AEQ(o1,o3)) {
        if(!quiet)System.err.print("\nAfter optimization: Different outputs. ")
        return false
      }
      true
    } catch {
      case ex:ArrayIndexOutOfBoundsException =>
        //ignore
        true
      case e => 
        if(!quiet)System.err.print(e) 
        throw e
    }
  }
  def pull(program:List[Expr]):(Array[Byte],Array[Byte]) = {
    val arr =new Array[Byte](1000000)
    val ob = new ByteArrayOutputStream
    Runner.ticks = 15000
    var here = 0
    program foreach {
      e =>
      here = Runner.execute(optimizer, e, arr, here, new ByteArrayInputStream(input), ob)
    }
    (arr, ob.toByteArray)
  }
}