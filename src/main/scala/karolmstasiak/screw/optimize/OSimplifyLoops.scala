package karolmstasiak.screw.optimize
import karolmstasiak.screw._

trait OSimplifyLoops {
  def simplifyLoops(l:List[Expr]):List[Expr] = {
    var ll:List[Expr]=l
    var ok:List[Expr]=Nil
    while(ll!=Nil) {
      ll match {
        case While(where, ys)::xs 
        if ys.foldLeft(true)((b,e) => b && e.isInstanceOf[SetRelative]) &&
           ys.contains(SetRelative(where,0)) && 
           Set(ys map {_.asInstanceOf[SetRelative].where}).size == ys.size
        => 
          ll = IfAndZero(where, ys filter {_.asInstanceOf[SetRelative].where != where})::ll
        case IfAndZero(where, ys)::IncreaseRelative(where2,v)::xs if where==where2 => 
          ll = IfAndZero(where, ys)::SetRelative(where2,v)::xs
        case While(where, ys)::IncreaseRelative(where2,v)::xs if where==where2 => 
          ll = While(where, ys)::SetRelative(where2,v)::xs
        case While(where, IncreaseRelative(where2,x)::Nil)::xs if x%2!=0 && where==where2 => 
          ll = SetRelative(where,0)::xs
        /*case While(where, IncreaseRelative(where2,-1)::Shift(d1)::IncreaseRelative(where3,x2)::Shift(d2)::Nil)::xs if where2+d1!=where3+d2 && where==where2 => 
          ok = MoveAddingRelative(where2, where3+d1, x2)::ok
          ll = xs
        case While(where, Shift(d1)::IncreaseRelative(where2,x2)::Shift(d2)::IncreaseRelative(where3,-1)::Nil)::xs if where2+d2!=where3+d1 && where==where2 => 
          ok = MoveAddingRelative(where2, where3+d1, x2)::ok
          ll = xs*/
        case While(where,expr)::xs if expr.size>=1 && expr.reverse.head == SetRelative(where,0) =>
          ll = IfAndZero(where, expr.reverse.tail.reverse)::xs
        case While(where, IncreaseRelative(where2,-1)::IncreaseRelative(d,x2)::Nil)::xs if where==where2 && where!=d => 
          ok = MoveAddingRelative(where,d,x2)::ok
          ll = xs
        case While(where, IncreaseRelative(d,x2)::IncreaseRelative(where2,-1)::Nil)::xs if where==where2 && where!=d => 
          ok = MoveAddingRelative(where,d,x2)::ok
          ll = xs
        case While(where, IncreaseRelative(d1,scale1)::IncreaseRelative(d2,scale2)::IncreaseRelative(where2,-1)::Nil)::xs 
        if where==where2 && 
           where!=d1  &&
           where!=d2
        => 
          ok = CopyAddingRelative(where,d1,scale1)::ok
          ok = MoveAddingRelative(where,d2,scale2)::ok
          ll = xs
        case While(where, IncreaseRelative(where2,-1)::IncreaseRelative(d1,scale1)::IncreaseRelative(d2,scale2)::Nil)::xs 
        if where==where2 && 
           where!=d1  &&
           where!=d2
        => 
          ok = CopyAddingRelative(where,d1,scale1)::ok
          ok = MoveAddingRelative(where,d2,scale2)::ok
          ll = xs
        case While(where, IncreaseRelative(d1,scale1)::IncreaseRelative(where2,-1)::IncreaseRelative(d2,scale2)::Nil)::xs 
        if where==where2 && 
           where!=d1  &&
           where!=d2
        => 
          ok = CopyAddingRelative(where,d1,scale1)::ok
          ok = MoveAddingRelative(where,d2,scale2)::ok
          ll = xs
        case While(0, IncreaseRelative(0,x1)::Shift(jump)::IncreaseRelative(0,x2)::Nil)::xs if (x1+x2).toByte==0 => 
          ok = IncreaseRelative(0,x1)::ok
          ok = Seek(x1,jump)::ok
          ok = SetRelative(0,0)::ok
          ll = xs
        case While(0, Write(1)::Shift(1)::Nil)::xs =>
          ok = WriteString()::ok
          ll = xs
        case While(where, Shift(d1)::Write(1)::Shift(d2)::Nil)::xs 
        if d2-d1 ==1 && where==d1=>  //TODO: czy dobrze??
          ok = Shift(d1)::ok
          ok = WriteString()::ok
          ok = Shift(-1)::ok
          ll = xs
        /*case While(_, Nil)::xs => 
          ok = Hang()::ok
          ll = xs*/
        case While(where, SetRelative(second, x)::SetRelative(where2,0)::Nil)::xs
        if where==where2 =>
          ll = IfAndZero(where, SetRelative(second, x)::Nil)::xs
          
        case While(where, IncreaseRelative(where2,dx)::SetRelative(other,value)::Nil)::xs
        if where==where2 && other!=where && (dx == 1 || dx == -1)=>
          ll = IfAndZero(where,SetRelative(other,value)::Nil)::xs
        case While(where, SetRelative(other,value)::IncreaseRelative(where2,dx)::Nil)::xs
        if where==where2 && other!=where && (dx == 1 || dx == -1)=>
          ll = IfAndZero(where,SetRelative(other,value)::Nil)::xs
          
        //trywialne warianty
        case While(where, other)::xs => 
          ok = While(where, simplifyLoops(other))::ok
          ll = xs
        case IfAndZero(where, Nil)::xs =>
          ll = SetRelative(where,0)::xs
        case IfAndZero(where, other)::xs => 
          ok = IfAndZero(where, simplifyLoops(other))::ok
          ll = xs
        case IfElseAndZero(where, other, other2)::xs => 
          ok = IfElseAndZero(where, simplifyLoops(other), simplifyLoops(other2))::ok
          ll = xs
        case x::xs => 
          ok = x::ok
          ll=xs
        case Nil => ()
      }
    }
    return ok reverse_::: ll
  }
}