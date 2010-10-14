package Syb3

object syb3varTC {
  
  trait TC {
    type A
  }
  
// Getting around run-time composition of traits!
  trait GMapQ[ctx <: TC,a] {
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => a => List[r]
  }
  
  trait GMapQChar[ctx <: TC] extends GMapQ[ctx,Char] {
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => Char => List[r] =
      f => n => Nil
  }
  
  abstract case class Data[ctx <: TC,a](implicit gmap : GMapQ[ctx,a]) {
    self : ctx {type A=a} =>
    
    def me = self
    
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => a => List[r] = 
      gmap.gmapQ[r]
  }
  
  implicit def gmapChar[ctx <: TC] : GMapQ[ctx,Char] = new GMapQChar[ctx](){} 

  /*
  trait Size extends Data[Size,a] with TC {
    self : Data[Size,a] with Size[a] =>
    type A=a
    
    def gsize : a => Int = t =>
      1 + sum(gmapQ[Int] (new {def apply[b](x:b)(implicit dt : Data[Size,b]) = dt.me.gsize(x)})(t))
  }
  
  
  def sum(list: List[Int]) = list.foldLeft(0)(_+_)
  
  implicit def sizeAll[a](implicit gmap : GMapQ[Size,a]) : Data[Size,a] = 
    new Data[Size,a]()(gmap) with Size[a]
  
  def test(implicit size : Data[Size,Char]) = size.me.gsize('a')*/
}
