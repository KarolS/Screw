package karolmstasiak.screw.emitter

import java.io.PrintStream
import karolmstasiak.screw._

object PythonEmitter extends Emitter {
  def emitArr(index:Int):String={
    if(index==0) return "m[p]"
    if(index<0) return "m[p-"+(-index)+"]"
    "m[p+"+index+"]"
  }
  var version = 2
  def emit2(l:Expr, os:PrintStream, indent:Int){
    val tabs = "\t"*indent
    os.print(tabs)
    l match {
      case While(where,xs) => 
        os.println("while ("+emitArr(where)+"&255) != 0:")
        xs foreach (emit2(_, os, indent+1))
      case IfAndZero(where,xs) => 
        os.println("if ("+emitArr(where)+"&255) != 0 :")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println(emitArr(where)+" = 0")
      case IfElseAndZero(where,xs,ys) => 
        os.println("if ("+emitArr(where)+"&255) != 0 :")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("\t"+emitArr(where)+" = 0")
        os.print(tabs)
        os.println("else :")
        ys foreach (emit2(_, os, indent+1))
        
      case CopyRelative(from,to,1) => 
        os.println(emitArr(to)+" = "+emitArr(from)+"")
      case CopyRelative(from,to,scale) => 
        os.println(emitArr(to)+" = "+scale+"*"+emitArr(from)+"")
        
      case MoveRelative(from,to,1) => 
        os.println(emitArr(to)+" = "+emitArr(from)+"")
        os.print(tabs)
        os.println(emitArr(from)+" = 0;")
      case MoveRelative(from,to,scale) => 
        os.println(emitArr(to)+" = "+scale+"*"+emitArr(from)+"")
        os.print(tabs)
        os.println(emitArr(from)+" = 0")
        
      case CopyAddingRelative(from,to,1) => 
        os.println(emitArr(to)+" += "+emitArr(from)+"")
      case CopyAddingRelative(from,to,-1) => 
        os.println(emitArr(to)+" -= "+emitArr(from)+"")
      case CopyAddingRelative(from,to,scale) => 
        os.println(emitArr(to)+" += "+scale+"*"+emitArr(from)+"")
        
      case MoveAddingRelative(from,to,1) => 
        os.println(emitArr(to)+" += "+emitArr(from)+"")
        os.print(tabs)
        os.println(emitArr(from)+" = 0")
      case MoveAddingRelative(from,to,-1) => 
        os.println(emitArr(to)+" -= "+emitArr(from)+"")
        os.print(tabs)
        os.println(emitArr(from)+" = 0")
      case MoveAddingRelative(from,to,scale) => 
        os.println(emitArr(to)+" += "+scale+"*"+emitArr(from)+"")
        os.print(tabs)
        os.println(emitArr(from)+" = 0")
        
      case IncreaseRelative(where,v) if v<0=> 
        os.println(emitArr(where)+" -= "+(-v))
      case IncreaseRelative(where,v) => 
        os.println(emitArr(where)+" += "+v)
      case SetRelative(where,v) => 
        os.println(emitArr(where)+" = "+v)
      case SetRelative32(where,v) => 
        throw new IllegalStateException("32-bit operation are not allowed for Python output")
      case SetAt(where,v) => 
        os.println("m["+where+"] = "+v)
      case SetAt32(where,v) => 
        throw new IllegalStateException("32-bit operation are not allowed for Python output")
        
      case Shift(v) if v<0 => 
        os.println("p -= "+(-v))
      case Shift(v) => 
        os.println("p += "+v)
      case Seek(what,skip) if skip<0 => 
        os.println("while (m[p]&255) != "+what+": p -= "+(-skip))
      case Seek(what,skip) => 
        os.println("while (m[p]&255) != "+what+": p += "+skip+";")
      case Hang() => 
        os.println("while True: pass")
        
      case Read(1) => 
        os.println("READ")
      case Read(i) => 
        if(version == 2)os.println("for i in xrange(0,"+i+"): READ")
        else os.println("for i in range(0,"+i+"): READ")
      case WriteConst(b) => 
        os.println("write(chr("+b+"))")
        
      case Write(1) => 
        os.println("write(chr(m[p]&255))")
      case Write(i) => 
        if(version == 2)os.println("for i in xrange(0,"+i+"): write(chr(m[p]&255))")
        else os.println("for i in range(0,"+i+"): write(chr(m[p]&255))")
        
      case WriteAndShift(i) => 
        os.println("for i in m[p:p+("+i+")]: write(chr(m[i]&255))")
        os.print(tabs)
        os.println("p += "+i)
      case WriteString() => 
        os.println("while (m[p]&255) != 0: write(chr(m[i]&255)); p += 1")
      case PutBytes(b) => 
        os.print("write('")
        b foreach{
          b =>
          (b&0xff) match {
            case x if x<16 => os.print("\\x0"+x.toHexString)
            case 34 => os.print("\\x22")
            case 39 => os.print("\\x29")
            case x if x<32 || x>=127 => os.print("\\x"+x.toHexString)
            case x => os.print(x.toChar.toString)
          }
        }
        os.print("')")
    }
  }
  def emit(tapeLength:Int, l:List[Expr], os:PrintStream){
    os.println("if __name__=='__main__':\n\timport sys\n\twrite = sys.stdout.write\n\tp = 0\n\tm = [0]*"+tapeLength)
    l foreach {emit2(_, os, 1)}
  }
}