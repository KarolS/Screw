package karolmstasiak.screw.optimize
import karolmstasiak.screw._

trait OReorder {
  def reorder(l:List[Expr]):List[Expr] = {
    var ll:List[Expr]=l
    var ok:List[Expr]=Nil
    while(ll!=Nil){
      ll match { 
        case Shift(d)::something::xs 
        if something.isInstanceOf[WriteConst] || something.isInstanceOf[PutBytes]
        => 
          ok = something::ok
          ll = (Shift(d)::xs)
        case Shift(d)::IncreaseRelative(t,x)::xs => 
          ok = IncreaseRelative(d+t,x)::ok
          ll = (Shift(d)::xs)
        case Shift(d)::SetRelative(t,x)::xs => 
          ok = SetRelative(d+t,x)::ok
          ll = (Shift(d)::xs)
        case Shift(d)::SetRelative32(t,x)::xs => 
          ok = SetRelative32(d+t,x)::ok
          ll = (Shift(d)::xs)
        case SetRelative(t1,x1)::SetRelative(t2,x2)::xs if t1>t2=> 
          ok = SetRelative(t2,x2)::ok
          ll = SetRelative(t1,x1)::xs
        case Shift(d)::MoveRelative(f,t,x)::xs => 
          ok = MoveRelative(d+f,d+t,x)::ok
          ll = (Shift(d)::xs)
        case Shift(d)::CopyRelative(f,t,x)::xs =>  
          ok = CopyRelative(d+f,d+t,x)::ok
          ll = (Shift(d)::xs)
        case Shift(d)::MoveAddingRelative(f,t,x)::xs =>  
          ok = MoveAddingRelative(d+f,d+t,x)::ok
          ll = (Shift(d)::xs)
        case Shift(d)::CopyAddingRelative(f,t,x)::xs =>  
          ok = CopyAddingRelative(d+f,d+t,x)::ok
          ll = (Shift(d)::xs)
        case Shift(d)::While(where, ys)::xs =>  
          ok = While(where+d, reorder(Shift(d)::ys ::: Shift(-d)::Nil)) :: ok
          ll = Shift(d)::xs
        case Shift(d)::IfAndZero(where, ys)::xs =>  
          ok = IfAndZero(where+d, reorder(Shift(d)::ys ::: Shift(-d)::Nil)) :: ok
          ll = Shift(d)::xs
        case Shift(d)::Shift(t)::xs =>
          ll = (Shift(d+t)::xs)
        case Shift(0)::xs => ll=xs
        case While(where, other)::xs =>  
          ok = While(where, reorder(other))::ok
          ll = xs
        case IfAndZero(where, other)::xs =>  
          ok = IfAndZero(where, reorder(other))::ok
          ll = xs
        case IfElseAndZero(where, Shift(d1)::other, Shift(d2)::other2)::xs 
        if d1==d2 =>  
          ok = Shift(d1)::ok
          ll = IfElseAndZero(where-d1, other, other2)::xs
        case IfElseAndZero(where, Shift(d1)::other, Shift(d2)::other2)::xs 
        if d1!=d2 =>  
          ok = Shift(d2)::ok
          ll = IfElseAndZero(where-d2, Shift(d1-d2)::other, other2)::xs
        case IfElseAndZero(where, other, other2)::xs =>  
          ok = IfElseAndZero(where, reorder(other), reorder(other2))::ok
          ll = xs
        case x::xs =>  
          ok = x::ok
          ll = xs
        case Nil => Nil
      }
    }
    return ok reverse_::: ll
  }
}