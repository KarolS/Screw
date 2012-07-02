package karolmstasiak.screw.optimize
import karolmstasiak.screw._


import scala.Array.canBuildFrom

trait OLinearize {
  def linearize(l:List[Expr]):List[Expr] = {
    var ll:List[Expr]=l
    var ok:List[Expr]=Nil
    while(ll!=Nil){
      ll match {
        case Shift(x)::Shift(y)::xs => 
          ll=Shift(x+y)::xs
        case Shift(0)::xs => 
          ll=xs
        case Read(x)::Read(y)::xs => 
          ll=(Read(x+y)::xs)
        case Write(x)::Write(y)::xs => 
          ll=(Write(x+y)::xs)
        case While(where1, While(where2, other)::Nil)::xs 
        if where1==where2 => 
          ll=(While(where1, other)::xs)
        case While(w, ys)::xs => 
          ok = While(w, linearize(ys))::ok ; ll=xs
        case IfAndZero(w, ys)::xs => 
          ok = IfAndZero(w, linearize(ys))::ok ; ll=xs
        case IfElseAndZero(w, ys, zs)::xs => 
          ok = IfElseAndZero(w, linearize(ys), linearize(zs))::ok ; ll=xs
          
        //łączenie i segregowanie tego samego typu
        case SetRelative(t,x)::SetRelative(t2,y)::xs if t==t2 => 
          ll = SetRelative(t,y)::xs
        case SetRelative(t,x)::SetRelative(t2,y)::xs if t>t2 => 
          ll = SetRelative(t2,y)::SetRelative(t,x)::xs
        case IncreaseRelative(t,x)::IncreaseRelative(t2,y)::xs if t==t2 => 
          ll = IncreaseRelative(t,(x+y)toByte)::xs
        case IncreaseRelative(t,x)::IncreaseRelative(t2,y)::xs if t>t2 => 
          ll = IncreaseRelative(t2,y)::IncreaseRelative(t,x)::xs
        case IncreaseRelative(t,0)::xs => ll=xs
        
        //bardzie skomplikowane przypadki
        case SetRelative(f,x)::CopyRelative(from,to,scale)::xs if from == f =>
          ll =  SetRelative(f,x) :: SetRelative(to,(x*scale)toByte)::xs
        case SetRelative(f,x)::MoveRelative(from,to,scale)::xs if from == f =>
          ll =  SetRelative(f,0) :: SetRelative(to,(x*scale)toByte)::xs
        case SetRelative(f,x)::CopyAddingRelative(from,to,scale)::xs if from == f =>
          ll =  SetRelative(f,x) :: IncreaseRelative(to,(x*scale)toByte)::xs
        case SetRelative(f,x)::MoveAddingRelative(from,to,scale)::xs if from == f =>
          ll =  SetRelative(f,0) :: IncreaseRelative(to,(x*scale)toByte)::xs
          
        case SetRelative(f,x)::SetRelative(q1,q2)::CopyRelative(from,to,scale)::xs 
        if from == f && f!=q1 && to!=q1 =>
          ll =  SetRelative(f,x) :: SetRelative(q1,q2)::SetRelative(to,(x*scale)toByte)::xs
        case SetRelative(f,x)::SetRelative(q1,q2)::MoveRelative(from,to,scale)::xs 
        if from == f && f!=q1 && to!=q1 =>
          ll =  SetRelative(f,0) :: SetRelative(q1,q2)::SetRelative(to,(x*scale)toByte)::xs
        case SetRelative(f,x)::SetRelative(q1,q2)::CopyAddingRelative(from,to,scale)::xs 
        if from == f && f!=q1 && to!=q1 =>
          ll =  SetRelative(f,x) :: SetRelative(q1,q2)::IncreaseRelative(to,(x*scale)toByte)::xs
        case SetRelative(f,x)::SetRelative(q1,q2)::MoveAddingRelative(from,to,scale)::xs 
        if from == f && f!=q1 && to!=q1 =>
          ll =  SetRelative(f,0) :: SetRelative(q1,q2)::IncreaseRelative(to,(x*scale)toByte)::xs
          
        case SetRelative(t,x)::IncreaseRelative(t2,y)::xs if t==t2 => 
          ll = (SetRelative(t,(x+y)toByte)::xs)
        case SetRelative(t,x)::IncreaseRelative(t2,y)::xs if t>t2 => 
          ll = IncreaseRelative(t2,y)::SetRelative(t,x)::xs
        case SetRelative(t,0)::CopyAddingRelative(from, to, scale)::xs if t==to => 
          ll = (CopyRelative(from, to, scale)::xs)
        case SetRelative(t,0)::MoveAddingRelative(from, to, scale)::xs if t==to => 
          ll = (MoveRelative(from, to, scale)::xs)
        case MoveAddingRelative(from1,to1,scale1)::CopyRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyAddingRelative(from1,to1,scale1)::CopyRelative(from2,to2,scale2)::xs
        case MoveAddingRelative(from1,to1,scale1)::CopyAddingRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyAddingRelative(from1,to1,scale1)::CopyRelative(from2,to2,scale2)::xs
        case MoveAddingRelative(from1,to1,scale1)::MoveRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyAddingRelative(from1,to1,scale1)::MoveRelative(from2,to2,scale2)::xs
        case MoveAddingRelative(from1,to1,scale1)::MoveAddingRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyAddingRelative(from1,to1,scale1)::MoveRelative(from2,to2,scale2)::xs
        case MoveRelative(from1,to1,scale1)::CopyRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyRelative(from1,to1,scale1)::CopyRelative(from2,to2,scale2)::xs
        case MoveRelative(from1,to1,scale1)::CopyAddingRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyRelative(from1,to1,scale1)::CopyRelative(from2,to2,scale2)::xs
        case MoveRelative(from1,to1,scale1)::MoveRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyRelative(from1,to1,scale1)::MoveRelative(from2,to2,scale2)::xs
        case MoveRelative(from1,to1,scale1)::MoveAddingRelative(from2,to2,scale2)::xs if from1==to2 =>
          ll = CopyRelative(from1,to1,scale1)::MoveRelative(from2,to2,scale2)::xs
        
        case MoveRelative(from, to1, scale)::SetRelative(to2,value)::xs  if to1==to2 =>
          ll = SetRelative(from,0)::SetRelative(to2,value)::xs
        case MoveAddingRelative(from, to1, scale)::SetRelative(to2,value)::xs  if to1==to2 =>
          ll = SetRelative(from,0)::SetRelative(to2,value)::xs
        case MoveRelative(from1, to1, scale)::SetRelative(from2,value)::xs  if from1==from2 =>
          ll = CopyRelative(from1, to1, scale)::SetRelative(from2,value)::xs
        case MoveAddingRelative(from1, to1, scale)::SetRelative(from2,value)::xs  if from1==from2 =>
          ll = CopyAddingRelative(from1, to1, scale)::SetRelative(from2,value)::xs
        case CopyRelative(from, to1, scale)::SetRelative(to2,value)::xs  if to1==to2 =>
          ll = SetRelative(to2,value)::xs
        case CopyAddingRelative(from, to1, scale)::SetRelative(to2,value)::xs  if to1==to2 =>
          ll = SetRelative(to2,value)::xs
          
        //pętle zawsze true i zawsze false
        case SetRelative(t,0)::While(where,_)::xs if t==where => 
          ll=(SetRelative(t,0)::xs)
        case SetRelative(t,0)::IfAndZero(where,_)::xs if t==where => 
          ll=(SetRelative(t,0)::xs)
        case SetRelative(t,a)::IfAndZero(where,ys)::xs if t==where && a!=0 => 
          ll=(SetRelative(t,a)::ys:::SetRelative(t,0)::xs)
        case SetRelative(t,0)::IfElseAndZero(where,ts,fs)::xs if t==where => 
          ll=(SetRelative(t,0)::fs:::xs)
        case SetRelative(t,a)::IfElseAndZero(where,ts,fs)::xs if t==where && a!=0 => 
          ll=(SetRelative(t,a)::ts:::SetRelative(t,0)::xs)
          
        //IO
        case SetRelative(0,c)::Write(n)::xs =>{
          ll=xs
          for (_ <- 0 until n) { ll=WriteConst(c)::ll }
          ll = SetRelative(0,c)::ll
        }
        case WriteConst(c1)::WriteConst(c2)::xs =>
          ll=PutBytes(Array(c1,c2))::xs
        case WriteConst(c)::PutBytes(arr)::xs =>
          ll=PutBytes(Array(c)++arr)::xs
        case PutBytes(arr)::WriteConst(c)::xs =>
          ll=PutBytes(arr++Array(c))::xs
        case PutBytes(arr1)::PutBytes(arr2)::xs =>
          ll=PutBytes(arr1++arr2)::xs
        case MoveRelative(from, to, scale)::While(where,_)::xs if from==where => ll=(MoveRelative(from, to, scale)::xs)
        case Write(1)::Shift(1)::xs =>
          ll = WriteAndShift(1)::xs
        case WriteAndShift(x)::WriteAndShift(y)::xs =>
          ll = WriteAndShift(x+y)::xs
        case WriteAndShift(x)::Write(1)::Shift(1)::xs =>
          ll = WriteAndShift(x+1)::xs
        case WriteAndShift(x)::Write(1)::Shift(d)::xs=>
          ll = WriteAndShift(x+1)::Shift(d-1)::xs
        case x::xs => ok=x::ok ; ll=xs
        case Nil => Nil
      }
    }
    return ok reverse_::: ll
  }

}