package karolmstasiak.screw
import java.io._
import karolmstasiak.screw.optimize._
object Runner{
  var ticks = 2000000000
  def execute(optimizer: Optimizer, e:Expr, arr:Array[Byte], here:Int, is:InputStream, os:OutputStream):Int={
    if (ticks<=0) return here
    ticks -= 1
    e match{
      case While(where, expr)=> 
        var newHere= here
        while(arr(where+newHere)!=0) {
          for(a<-expr){
            if(ticks<=0) return here
            newHere=execute(optimizer, a,arr,newHere,is,os)
          }
        }
        return newHere
      case IfAndZero(where, expr)=> 
        var newHere= here
        if(arr(where+newHere)!=0) {
          for(a<-expr){
            if(ticks<=0) return here
            newHere=execute(optimizer, a,arr,newHere,is,os)
            arr(where+here)=0
          }
        }
        return newHere
      case IfElseAndZero(where, texpr, fexpr)=> 
        var newHere= here
        if(arr(where+newHere)!=0) {
          for(a<-texpr){
            if(ticks<=0) return here
            newHere=execute(optimizer, a,arr,newHere,is,os)
            arr(where+here)=0
          }
        }
        else{
          for(a<-fexpr){
            if(ticks<=0) return here
            newHere=execute(optimizer, a,arr,newHere,is,os)
          }
        }
        return newHere
      case Shift(x) => return here+x
      case Seek(v,skip) => {
        var newHere = here
        while(arr(newHere)!=v){
          newHere+=skip
        }
        return newHere
      }
      case SetAt(there,x)=>arr(there)=x; return here
      case SetRelative(there,x) => arr(here+there) = x; return here
      case IncreaseRelative(there,x) => arr(here+there) = (arr(here+there)+x)toByte; return here
      case CopyRelative(from,to,scale) => arr(to+here)=(arr(from+here)*scale)toByte; return here
      case MoveRelative(from,to,scale) => arr(to+here)=(arr(from+here)*scale)toByte; arr(from+here)=0; return here
      case CopyAddingRelative(from,to,scale) => arr(to+here)=(arr(to+here)+arr(from+here)*scale)toByte; return here
      case MoveAddingRelative(from,to,scale) => arr(to+here)=(arr(to+here)+arr(from+here)*scale)toByte; arr(from+here)=0; return here
      case Read(count) => for(_<-0 until count) arr(here) = is.read.toByte ; return here
      case WriteConst(b) => os write b ; return here
      case Write(count) => for(_<-0 until count) os write arr(here) ; return here
      case WriteAndShift(count) => for(i<-0 until count) os write arr(here+i) ; return here+count
      case PutBytes(bytes) => os.write(bytes) ; return here
      case WriteString() => {
        var newHere = here
        while(arr(newHere)!=0){
          os.write(arr(newHere))
          newHere+=1
        }
        return newHere
      }
      case Hang() => return here
      case SetRelative32(where, value) =>{
        optimizer.littleEndian match {
          case Some(true) => {
            arr(here+where+0) = (value).toByte
            arr(here+where+1) = (value>>>8).toByte
            arr(here+where+2) = (value>>>16).toByte
            arr(here+where+3) = (value>>>24).toByte
          }
          case Some(false) => {
            arr(here+where+3) = (value).toByte
            arr(here+where+2) = (value>>>8).toByte
            arr(here+where+1) = (value>>>16).toByte
            arr(here+where+0) = (value>>>24).toByte
          }
          case None => throw new IllegalStateException("Trying to emulate 32-bit instruction without endianness set")
        }
        here
      }
      case SetAt32(where, value) =>{
        optimizer.littleEndian match {
          case Some(true) => {
            arr(where+0) = (value).toByte
            arr(where+1) = (value>>>8).toByte
            arr(where+2) = (value>>>16).toByte
            arr(where+3) = (value>>>24).toByte
          }
          case Some(false) =>{
            arr(where+3) = (value).toByte
            arr(where+2) = (value>>>8).toByte
            arr(where+1) = (value>>>16).toByte
            arr(where+0) = (value>>>24).toByte
          }
          case None => throw new IllegalStateException("Trying to emulate 32-bit instruction without endianness set")
        }
        here
      }
    }
  }
}
