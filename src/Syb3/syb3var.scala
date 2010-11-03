package Syb3;
//package syb3

object syb3var {
  // Getting around run-time composition of traits!
  trait GMapQ[ctx[_],a] {
    self : ctx[a] =>
    
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : GMapQ[ctx,b] with ctx[b]) : r} => a => List[r]
  }
  
  trait GMapQChar[ctx[_]] extends GMapQ[ctx,Char] {
    self : ctx[Char] =>
    
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : GMapQ[ctx,b] with ctx[b]) : r} => Char => List[r] =
      f => n => Nil
  }
  
  abstract case class GMapQList[ctx[_],a](implicit d : GMapQ[ctx,a] with ctx[a]) extends GMapQ[ctx,List[a]] {
    self : ctx[List[a]] =>
    
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : GMapQ[ctx,b] with ctx[b]) : r} => List[a] => List[r] =
      f => {
        case Nil    => Nil
        //case x::xs  => f.apply(x)(d) :: f.apply(xs)(this)
        case _ => Nil
      }
  }
  
  /*
  implicit def gmapChar[ctx[_]] : GMapQ[ctx,Char] = new GMapQChar[ctx](){} 
  implicit def gmapList[ctx[_],a](implicit d : GMapQ[ctx,a]) : GMapQ[ctx,List[a]] = new GMapQList[ctx,a]()(d){}
  
  abstract case class Data[ctx[_],a](implicit gmap : GMapQ[ctx,a]) {
    self : ctx[a] =>
    
    def me = self
    
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => a => List[r] = 
      gmap.gmapQ[r]
  }

  trait Size[a] extends Data[Size,a] {
    self : Data[Size,a] with Size[a] =>
    
    def gsize : a => Int = t =>
      1 + sum(gmapQ[Int] (new {def apply[b](x:b)(implicit dt : Data[Size,b]) = dt.me.gsize(x)})(t))
  }
  
  def sum(list: List[Int]) = list.foldLeft(0)(_+_)
  
  implicit def size[a](implicit gmap : GMapQ[Size,a]) : Data[Size,a] = 
    new Data[Size,a]()(gmap) with Size[a]
  
  case class SizeList[a]()(implicit d : Size[a]) extends Size[List[a]] {
    override def gsize : List[a] => Int = {
      case Nil    => 0
      case x::xs  => d.gsize(x) + gsize(xs)
    }
  }
  
  def test(implicit size : Data[Size,Char]) = size.me.gsize('a')
  */
}
