package karolmstasiak.screw.emitter

import java.io.PrintStream
import karolmstasiak.screw._

object JavaEmitter extends Emitter {
  def emitArr(index:Int):String={
    if(index==0) return "m[p]"
    if(index<0) return "m[p-"+(-index)+"]"
    "m[p+"+index+"]"
  }
  def emit2(l:Expr, os:PrintStream, indent:Int){
    val tabs = "\t"*indent
    os.print(tabs)
    l match {
      case While(where,xs) => 
        os.println("while ("+emitArr(where)+" != 0) {")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("}")
      case IfAndZero(where,xs) => 
        os.println("if ("+emitArr(where)+" != 0) {")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("}")
        os.print(tabs)
        os.println(emitArr(where)+" = 0;")
      case IfElseAndZero(where,xs,ys) => 
        os.println("if ("+emitArr(where)+" != 0) {")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println(emitArr(where)+" = 0;")
        os.print(tabs)
        os.println("} else {")
        ys foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("}")
        
      case CopyRelative(from,to,1) => 
        os.println(emitArr(to)+" = "+emitArr(from)+";")
      case CopyRelative(from,to,scale) => 
        os.println(emitArr(to)+" = "+scale+"*"+emitArr(from)+";")
        
      case MoveRelative(from,to,1) => 
        os.println(emitArr(to)+" = "+emitArr(from)+";")
        os.print(tabs)
        os.println(emitArr(from)+" = 0;")
      case MoveRelative(from,to,scale) => 
        os.println(emitArr(to)+" = "+scale+"*"+emitArr(from)+";")
        os.print(tabs)
        os.println(emitArr(from)+" = 0;")
        
      case CopyAddingRelative(from,to,1) => 
        os.println(emitArr(to)+" += "+emitArr(from)+";")
      case CopyAddingRelative(from,to,-1) => 
        os.println(emitArr(to)+" -= "+emitArr(from)+";")
      case CopyAddingRelative(from,to,scale) => 
        os.println(emitArr(to)+" += "+scale+"*"+emitArr(from)+";")
        
      case MoveAddingRelative(from,to,1) => 
        os.println(emitArr(to)+" += "+emitArr(from)+";")
        os.print(tabs)
        os.println(emitArr(from)+" = 0;")
      case MoveAddingRelative(from,to,-1) => 
        os.println(emitArr(to)+" -= "+emitArr(from)+";")
        os.print(tabs)
        os.println(emitArr(from)+" = 0;")
      case MoveAddingRelative(from,to,scale) => 
        os.println(emitArr(to)+" += "+scale+"*"+emitArr(from)+";")
        os.print(tabs)
        os.println(emitArr(from)+" = 0;")
        
      case IncreaseRelative(where,1) => 
        os.println("++"+emitArr(where)+";")
      case IncreaseRelative(where,-1) => 
        os.println("--"+emitArr(where)+";")
      case IncreaseRelative(where,v) if v<0=> 
        os.println(emitArr(where)+" -= "+(-v)+";")
      case IncreaseRelative(where,v) => 
        os.println(emitArr(where)+" += "+v+";")
      case SetRelative(where,v) => 
        os.println(emitArr(where)+" = "+v+";")
      case SetRelative32(where,v) => 
        throw new IllegalStateException("32-bit operation are not allowed for Java output")
      case SetAt(where,v) => 
        os.println("m["+where+"] = "+v+";")
      case SetAt32(where,v) => 
        throw new IllegalStateException("32-bit operation are not allowed for Java output")
        
      case Shift(1) => 
        os.println("++p;")
      case Shift(-1) => 
        os.println("--p;")
      case Shift(v) if v<0 => 
        os.println("p -= "+(-v)+";")
      case Shift(v) => 
        os.println("p += "+v+";")
      case Seek(what,skip) if skip<0 => 
        os.println("while(m[p]!="+what+") p -= "+(-skip)+";")
      case Seek(what,skip) => 
        os.println("while(m[p]!="+what+") p += "+skip+";")
      case Hang() => 
        os.println("while(1);")
        
      case Read(1) => 
        os.println("READ;")
      case Read(i) => 
        os.println("for(int i=0; i<"+i+"; ++i) READ;")
        
      case WriteConst(b) => 
        os.println("System.out.write("+b+");")
        
      case Write(1) => 
        os.println("System.out.write(m[p]);")
      case Write(i) => 
        os.println("for(int i=0; i<"+i+"; ++i) System.out.write(m[p]);")
        
      case WriteAndShift(i) => 
        os.println("System.out.write(m, p, "+i+");")
        os.print(tabs)
        os.println("p += "+i+";")
      case WriteString() => 
        os.println("while(m[p]!=0) {System.out.write(m[p]); p++;};")
      case PutBytes(b) => 
        os.print("System.out.write(new byte[]{")
        os.print('"')
        b foreach{
          b =>
          os.print(b)
          os.print(", ")
        }
        os.print("});")
    }
  }
  def emit(tapeLength:Int, l:List[Expr], os:PrintStream){
    os.println("public class BrainFuck {\n\tpublic static void main(String[] args) {\n\t\tbyte[] m=new byte["+tapeLength+"];\n\t\tint p=0;")
    l foreach {emit2(_, os, 2)}
    os.println("\t}\n}");
  }
}