package karolmstasiak.screw

sealed abstract class Expr{
  def isPure:Boolean
  def isNonIO:Boolean = true
  def isNonRead:Boolean = true
  def getShift:Option[Int] = Some(0)
  def getLevel:Int = 0
  def hasShift = false
  def isMagic = false
}
case class While (val where:Int, val expr:List[Expr])extends Expr{
  override def isPure = expr.map(_.isPure).foldLeft(true)(_&&_)
  override def isNonIO = expr.map(_.isNonIO).foldLeft(true)(_&&_)
  override def getShift:Option[Int] = {
    var sum = 0
    expr map (_.getShift) foreach{
      case Some(i) => sum+=i
      case None => return None
    }
    if(sum!=0) None
    else Some(0)
  }
  override def hasShift = expr.foldLeft(true){(b,e)=>b&&e.hasShift}
  override def getLevel = if (expr==Nil) 100 else 1+ (expr map {_.getLevel} max)
}
case class IfAndZero (val where:Int, val expr:List[Expr])extends Expr{
  override def isPure = (true/:expr.map(_.isPure))(_&&_)
  override def isNonIO = (true/:expr.map(_.isNonIO))(_&&_)
  override def getShift:Option[Int] = {
    var sum = 0
    expr map (_.getShift) foreach{
      case Some(i) => sum+=i
      case None => return None
    }
    if(sum!=0) None
    else Some(0)
  }
  override def hasShift = expr.foldLeft(true){(b,e)=>b&&e.hasShift}
  override def getLevel = 1+ (expr map {_.getLevel} max)
}

case class IfElseAndZero (val where:Int, val trueExpr:List[Expr], val falseExpr:List[Expr])extends Expr{
  override def isPure = trueExpr.map(_.isPure).foldLeft(true)(_&&_)&&falseExpr.map(_.isPure).foldLeft(true)(_&&_)
  override def isNonIO = trueExpr.map(_.isNonIO).foldLeft(true)(_&&_)&&falseExpr.map(_.isNonIO).foldLeft(true)(_&&_)
  override def getShift:Option[Int] = {
    var sumT = 0
    trueExpr map (_.getShift) foreach{
      case Some(i) => sumT+=i
      case None => return None
    }
    if(sumT!=0) None
    else{
      var sumF = 0
      falseExpr map (_.getShift) foreach{
        case Some(i) => sumF+=i
        case None => return None
      }
      if(sumF!=0) None
      else Some(0)
    }
  }
  override def hasShift = trueExpr.foldLeft(true){(b,e)=>b&&e.hasShift}||falseExpr.foldLeft(true){(b,e)=>b&&e.hasShift}
  override def getLevel = 1+ ((trueExpr map {_.getLevel} max)max(falseExpr map {_.getLevel} max))
}

case class Shift (val i:Int)extends Expr {
  override def isPure = true
  override def getShift = Some (i)
  override def hasShift = true
  override def isMagic = true
}
case class Seek(val value:Byte, val step:Int) extends Expr {
  override def isPure = true
  override def getShift = None
  override def getLevel = 1
}
case class Hang ()extends Expr {
  override def isPure = true
  override def isNonIO = false
  override def getLevel = 100
}
case class Read (val i:Int)extends Expr {
  override def isPure = false; 
  override def isNonIO = false
  override def isNonRead = false
  override def getLevel = 100
}
case class Write (val i:Int)extends Expr {
  override def isPure = false; 
  override def isNonIO = false
  override def getLevel = 1
  override def isMagic = true
}
case class WriteConst (val i:Byte)extends Expr {
  override def isPure = false; 
  override def isNonIO = false
  override def getLevel = 1
  override def isMagic = true
}
case class SetRelative (val where:Int, val i:Byte)extends Expr {
  override def isPure = false
  override def isMagic = true
}
case class SetRelative32 (val where:Int, val i:Int)extends Expr {override def isPure = false}
case class SetAt32 (val where:Int, val i:Int)extends Expr {override def isPure = false}
//case class SetRelative16 (val where:Int, val i:Short)extends Expr {override def isPure = false}
//case class SetRelative64 (val where:Int, val i:Long)extends Expr {override def isPure = false}
case class IncreaseRelative (val where:Int, val i:Byte)extends Expr {
  override def isPure = false
  override def isMagic = true
}
case class SetAt (val where:Int, val i:Byte)extends Expr {override def isPure = false}
case class CopyRelative (val from:Int, val to:Int, val scale:Int)extends Expr {
  override def isPure = false
  override def isMagic = true
}
case class MoveRelative (val from:Int, val to:Int, val scale:Int)extends Expr {
  override def isPure = false
  override def isMagic = true
}
case class CopyAddingRelative (val from:Int, val to:Int, val scale:Int)extends Expr {
  override def isPure = false
  override def isMagic = true
}
case class MoveAddingRelative (val from:Int, val to:Int, val scale:Int)extends Expr {
  override def isPure = false
  override def isMagic = true
}
case class WriteAndShift(val count:Int)extends Expr {
  override def isPure = false; 
  override def isNonIO = false
  override def getLevel = 1
  override def getShift = Some(count)
}
case class WriteString()extends Expr {
  override def isPure = false; 
  override def isNonIO = false
  override def getLevel = 1
  override def getShift = None
}
case class PutBytes(val bytes:Array[Byte]) extends Expr {
  override def isPure = false
  override def isNonIO = false
  override def getLevel = 1
  override def toString = 
  "PutBytes(" + 
  (bytes.map {
    b=>
    val i = (b.toInt)&0xff
    if(i<16) "0"+i.toHexString
    else i.toHexString
  }.foldLeft("")(_+_)) + ")"
}

//TODO
