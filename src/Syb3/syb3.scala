package Syb3;
//package syb3

object syb3 {
  // Example where run-time composition of traits would be handy!
  
  
  trait Data[ctx[_],a] {
    self : ctx[a] =>
    def me = self
    
    trait ForallBC[W[_]] {
         def apply[B,C](w:W[B=>C], x : B) (implicit dt : Data[ctx,B]) : W[C]
    }
    
    trait ForallG[W[_]] {
        def apply[G](g :G) : W[G]
    }
    /**
     gfoldl :: Proxy ctx
            -> (forall b c. Data ctx b => w (b -> c) -> b -> w c)
            -> (forall g. g -> w g)
            -> a -> w a
    */
    
    def gfold[A,W[_]]: ForallBC[W]=>ForallG[W] =>  A => W[A]
    
    
    def gmapQ[r] : {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => a => List[r]
  }

  abstract case class DataChar[ctx[_]]() extends Data[ctx,Char] {
    self : ctx[Char] =>
    
    def gmapQ[r] = f => n => Nil
  } 
  
  abstract case class DataList[ctx[_],a](implicit d : Data[ctx,a]) extends Data[ctx,List[a]] {
    self : ctx[List[a]] =>
    
   /* def forallBC[W[_]] = new ForallBC[W] {
         def apply[B,C](w:W[B=>C], x : B) (implicit dt : Data[ctx,B]) : W[C] = w(x)
    }*/
    /*  trait ForallG[W[_]] {
        def apply[G](g :G) : W[G] = {
            x=>g(x)
        }
    }*/
    
   // def gold[A,W[_]] = 
    
    def gmapQ[r] = f => {
        case Nil    => Nil
        case x::xs  => List(f(x)(d),f(xs)(this))
    }
    
    
  }
  
  trait Size[a] extends Data[Size,a] {
    def gsize : a => Int = t =>
      1 + sum(gmapQ[Int] (new {def apply[b](x:b)(implicit dt : Data[Size,b]) = dt.me.gsize(x)})(t))
  }
  
 /* abstract case class SizeList[a]()(implicit d : Size[a]) extends DataList[Size,a]()(d) with Size[List[a]] {
    override def gsize = {
      case Nil => 0
      case x::xs => d.gsize(x) + gsize(xs)
    } 
  } */
  
  // inconvenient: Have to repeat code per each generic function.
/*  implicit def sizeChar : Size[Char] = 
    new DataChar[Size]() with Size[Char] 
  
  def alternativeList[a](implicit d : Size[a]) : Size[List[a]] = 
    new DataList[Size,a]()(d) with Size[List[a]]
  
  implicit def sizeList2[a](implicit d : Size[a]) : Size[List[a]] = 
    new SizeList[a]()(d) with Size[List[a]]
  */
  // The following is not possible.
  
  /*
  implicit def genericList[ctx[_],a](implicit d : ctx[a]) : Data[ctx,List[a]] = 
    new DataList[Size,a]()(d) with ctx[List[a]]
  */
                                                
  def test(implicit size : Size[Char]) = size.gsize('a')
  def test2(implicit size : Data[Size,Char]) = size.me.gsize('a')
  def test3(implicit size : Size[List[Char]]) = size.gsize(List('a','b'))
  
  def test4(implicit s1 : Size[Char], s2 : Size[List[Char]]) = 
    (s1.gsize('a'),s2.gsize(List('a','b')))
  
  def sum(list: List[Int]) = list.foldLeft(0)(_+_)
  
   /*
   trait A {def foo() : Int}
   trait B {}
   
   trait C extends B with A
   
   def test(x : A with B) = x.foo()
   
   
   trait T[A] {self : A => def me = self}
   
   trait TA extends T[TA] {self : TA => def foo() : Int}
   
   def test(x : T[TA]) = x.me.foo()
   
   trait T1[A] extends A*/
}
