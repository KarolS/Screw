package karolmstasiak.screw.optimize
import karolmstasiak.screw._


class Optimizer(
    val level:Int, 
    val littleEndian:Option[Boolean] = None, 
    val preinitializationLevel:Int = 3, 
    val doTrimUseless:Boolean = true) 
    extends OReorder 
    with OLinearize 
    with OSimplifyLoops
    with OInitialization 
    with OBulkSets 
    with OTrimUseless {

  private def optimize2(l:List[Expr], level:Int):List[Expr] = level match {
    case 0 => initialization(simplifyLoops(linearize(l)))
    case _ => trimUseless(bulkSets(linearize(simplifyLoops(reorder(simplifyLoops(linearize(optimize2(l,level-1))))))))
  }
  def optimize(l:List[Expr]):List[Expr] = optimize2(l,level)
}
