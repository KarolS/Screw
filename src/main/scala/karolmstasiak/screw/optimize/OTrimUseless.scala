package karolmstasiak.screw.optimize
import karolmstasiak.screw._

trait OTrimUseless {
  this: Optimizer=> 
  def trimUseless(l:List[Expr]):List[Expr] = {
    if (!doTrimUseless) return l
    var ll = l.reverse
    while (ll!=Nil) {
      ll match {
        case x::xs if x.isNonIO =>
          ll = xs
        case _ => return ll.reverse
      }
    }
    ll.reverse
  }

}