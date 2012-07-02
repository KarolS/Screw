package karolmstasiak.screw.optimize
import karolmstasiak.screw._

trait OBulkSets {
  this: Optimizer =>
  
  def bulkSets(l:List[Expr]):List[Expr] = {
    if(this.littleEndian==None) return l
    val littleEndian = this.littleEndian==Some(true)
    var ll:List[Expr]=l
    var ok:List[Expr]=Nil
    while(ll!=Nil){
      ll match { 
        case SetRelative(t0,x0)::SetRelative(t1,x1)::SetRelative(t2,x2)::SetRelative(t3,x3)::xs 
        if t1==t0+1 && t2 == t0+2 && t3 == t0+3
        =>
          ok = SetRelative32(t0, if (littleEndian) x0+(x1<<8)+(x2<<16)+(x3<<24) else x3+(x2<<8)+(x1<<16)+(x0<<24))::ok
          ll = xs
        case SetAt(t0,x0)::SetAt(t1,x1)::SetAt(t2,x2)::SetAt(t3,x3)::xs 
        if t0%4==0 && t1==t0+1 && t2 == t0+2 && t3 == t0+3
        =>
          ok = SetAt32(t0, if (littleEndian) x0+(x1<<8)+(x2<<16)+(x3<<24) else x3+(x2<<8)+(x1<<16)+(x0<<24))::ok
          ll = xs
        case While(where, ys)::xs => 
          ok = While(where, bulkSets(ys))::ok
          ll = xs
        case IfAndZero(where, ys)::xs => 
          ok = IfAndZero(where, bulkSets(ys))::ok
          ll = xs
        case x::xs =>
          ok = x::ok
          ll = xs
        case Nil => ()
      }
    }
    return ok reverse_::: ll
  }

}