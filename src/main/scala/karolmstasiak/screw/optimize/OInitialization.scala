package karolmstasiak.screw.optimize
import karolmstasiak.screw._


import java.io.ByteArrayOutputStream
import java.io.OutputStream

trait OInitialization {
  this: Optimizer =>
  def preinitializationLevel: Int
  
  def initialization(l:List[Expr]):List[Expr] = {
    if(preinitializationLevel<0) return l
    System.err.println(l.size+" tokens in the program, emulating...")
    val arr =new Array[Byte](1000000)
    val ob = new ByteArrayOutputStream
    var (here:Int, l2:List[Expr]) = initialization2(l,arr,0,ob)
    l2 = Shift(here)::l2
    for(x<-(0 until 1000000).reverse){
      if (arr(x)!=0){
        l2=SetAt(x,arr(x))::l2
      }
    }
    System.err.println()
    if(ob.size()>0) PutBytes(ob.toByteArray)::l2
    else l2
  }
  
  def initialization2(l:List[Expr], arr:Array[Byte], here:Int, ob:OutputStream):(Int, List[Expr])={
    l match {
      case x::xs if x.isNonRead && x.getLevel <= preinitializationLevel => {
        val here2 = Runner.execute(this, x, arr, here,System.in,ob)
        System.err.print(".")
        initialization2(xs,arr,here2,ob)
      }
      case _ => (here,l)
    }
  }
}