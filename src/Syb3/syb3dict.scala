package Syb3;
//package syb3

object syb3dict {
  abstract case class Data[ctx[_],a](implicit ctx : ctx[a]) {
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => a => List[r]
  }
  
  trait DataChar[ctx[_]] extends Data[ctx,Char] {
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => Char => List[r] =
      f => n => Nil
  }
  
  implicit def dataChar[ctx[_]](implicit ctx : ctx[Char]) : Data[ctx,Char] = 
    new Data[ctx,Char]()(ctx) with DataChar[ctx]
  
  trait Size[a] extends Data[Size,a] {
    def gsize : a => Int = t =>
      1 + sum(gmapQ[Int] (new {def apply[b](x:b)(implicit dt : Data[Size,b]) = dt.ctx.gsize(x)})(t))
  }
  
  //implicit def sizeAll[a] : Size[a] = new Data[Size,a]()(sizeAll[a]) with Size[a]
  
  def sum(list: List[Int]) = list.foldLeft(0)(_+_)
  
  def test(implicit size : Data[Size,Char]) = size.ctx.gsize('a')
  
  /*
  // Not needed
  class O {
   def apply[b](x:b)(implicit dt : Data[Size,b]) : Int = dt.me.gsize(x)
  }
  
  
  
  //implicit def dataSize[a] = new Size[a](){}
  
  
  */
}
