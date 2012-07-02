package karolmstasiak.screw.emitter
import java.io.PrintStream
import karolmstasiak.screw._

object CEmitter extends Emitter {
  def emit2(l:Expr, os:PrintStream, indent:Int){
    val tabs = "\t"*indent
    os.print(tabs)
    l match {
      case While(where,x::Nil) 
      if !x.isInstanceOf[While] &&
         !x.isInstanceOf[MoveRelative] &&
         !x.isInstanceOf[MoveAddingRelative] &&
         !x.isInstanceOf[Seek] &&
         !x.isInstanceOf[WriteAndShift] &&
         !x.isInstanceOf[WriteString] &&
         !x.isInstanceOf[Read] &&
         !x.isInstanceOf[Hang] &&
         !x.isInstanceOf[IfAndZero] &&
         true
      => 
        os.print("while (p["+where+"]) ")
        emit2(x, os, 0)
      case While(where,xs) => 
        os.println("while (p["+where+"]) {")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("}")
      case IfAndZero(where,xs) => 
        os.println("if (p["+where+"]) {")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("\tp["+where+"] = 0;")
        os.print(tabs)
        os.println("}")
      case IfElseAndZero(where,xs,ys) => 
        os.println("if (p["+where+"]) {")
        xs foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("\tp["+where+"] = 0;")
        os.print(tabs)
        os.println("} else {")
        ys foreach (emit2(_, os, indent+1))
        os.print(tabs)
        os.println("}")
       
      case CopyRelative(from,to,1) => 
        os.println("p["+to+"] = p["+from+"];")
      case CopyRelative(from,to,scale) => 
        os.println("p["+to+"] = "+scale+"*p["+from+"];")
        
      case MoveRelative(from,to,1) => 
        os.println("p["+to+"] = p["+from+"];")
        os.print(tabs)
        os.println("p["+from+"] = 0; /*move*/")
      case MoveRelative(from,to,scale) => 
        os.println("p["+to+"] = "+scale+"*p["+from+"];")
        os.print(tabs)
        os.println("p["+from+"] = 0; /*move*/")
        
      case CopyAddingRelative(from,to,1) => 
        os.println("p["+to+"] += p["+from+"];")
      case CopyAddingRelative(from,to,-1) => 
        os.println("p["+to+"] -= p["+from+"];")
      case CopyAddingRelative(from,to,scale) => 
        os.println("p["+to+"] += "+scale+"*p["+from+"];")
        
      case MoveAddingRelative(from,to,1) => 
        os.println("p["+to+"] += p["+from+"];")
        os.print(tabs)
        os.println("p["+from+"] = 0; /*move*/")
      case MoveAddingRelative(from,to,-1) => 
        os.println("p["+to+"] -= p["+from+"];")
        os.print(tabs)
        os.println("p["+from+"] = 0; /*move*/")
      case MoveAddingRelative(from,to,scale) => 
        os.println("p["+to+"] += "+scale+"*p["+from+"];")
        os.print(tabs)
        os.println("p["+from+"] = 0; /*move*/")
        
      case IncreaseRelative(where,1) => 
        os.println("++p["+where+"];")
      case IncreaseRelative(where,-1) => 
        os.println("--p["+where+"];")
      case IncreaseRelative(where,v) if v<0=> 
        os.println("p["+where+"] -= "+(-v)+";")
      case IncreaseRelative(where,v) => 
        os.println("p["+where+"] += "+v+";")
        
      case SetRelative(where,v) => 
        os.println("p["+where+"] = "+v+";")
      //case SetRelative16(where,v) => 
      //  os.println("*(uint16_t*)(p+"+where+") = "+v+";")
      case SetRelative32(where,v) => 
        os.println("*(uint32_t*)(p+"+where+") = "+v+";")
      //case SetRelative64(where,v) => 
      //  os.println("*(uint64_t*)(p+"+where+") = "+v+";")
      
      case SetAt(where,v) => 
        os.println("m["+where+"] = "+v+";")
      case SetAt32(where,v) => 
        os.println("*(uint32_t*)(m+"+where+") = "+v+";")
        
      case Shift(1) => 
        os.println("++p;")
      case Shift(-1) => 
        os.println("--p;")
      case Shift(v) if v<0 => 
        os.println("p -= "+(-v)+";")
      case Shift(v) => 
        os.println("p += "+v+";")
      case Seek(what,skip) if skip<0 => 
        os.println("while(*p!="+what+") p -= "+(-skip)+";")
      case Seek(what,skip) => 
        os.println("while(*p!="+what+") p += "+skip+";")
      case Hang() => 
        os.println("while(1);")
        
      case Read(1) => 
        os.println("READ;")
      case Read(i) => 
        os.println("for(int i=0; i<"+i+"; ++i) READ;")
        
      case WriteConst(b) => 
        os.println("fputc("+b+",stdout);")
        
      case Write(1) => 
        os.println("fputc(*p,stdout);")
      case Write(i) => 
        os.println("for(int i=0; i<"+i+"; ++i) fputc(*p, stdout);")
        
      case WriteAndShift(i) => 
        os.println("fwrite(p, 1, "+i+", stdout);")
        os.print(tabs)
        os.println("p += "+i+";")
      case WriteString() => 
        os.println("fputs(p, stdout);")
        os.print(tabs)
        os.println("p += strlen(p);")
      case PutBytes(b) => 
        os.print("fwrite(")
        os.print('"')
        b foreach{
          b=>
          (b&0xff) match {
            case x if x<16 => os.print("""" "\x0"""+x.toHexString+"""" """")
            case 34 => os.print("""" "\x22" """")
            case 39 => os.print("""" "\x29" """")
            case x if x<32 || x>=127 => os.print("""\x"""+x.toHexString+"""" """")
            case x => os.print(x.toChar.toString)
          }
        }
        os.print('"')
        os.println(", 1, "+b.size+", stdout);")
    }
  }
  private var heap = false
  override def setHeapTape(heap: Boolean) = this.heap = heap
  
  def emit(tapeLength:Int, l:List[Expr], os:PrintStream){
    os.println("#include <stdio.h>\n#include <stdint.h>\n#define READ *p = getchar()")
    if(heap){
      os.println("static uint8_t m["+tapeLength+"];\nint main(){\n\tregister uint8_t *p = m;")
      //TODO: change to calloc or malloc
    }
    else{
      os.println("static uint8_t m["+tapeLength+"];\nint main(){\n\tregister uint8_t *p = m;")
    }
    l foreach {emit2(_, os, 1)}
    os.println("\treturn 0;\n}");
  }
}
