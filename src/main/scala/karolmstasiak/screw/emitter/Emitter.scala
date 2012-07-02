package karolmstasiak.screw.emitter

import java.io.PrintStream
import karolmstasiak.screw.Expr

trait Emitter {
  def setHeapTape(heap: Boolean) {}
  def emit(tapeLength:Int, l:List[Expr], os:PrintStream)
}