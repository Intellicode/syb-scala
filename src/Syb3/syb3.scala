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
    
    def gfold[A,W[_]](f: ForallBC[W], k :ForallG[W], a:A):W[A]
    
    
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
    
    /*def gmapQ[r] = f => {
        case Nil    => Nil
        case x::xs  => List(f(x)(d),f(xs)(this))
        case _ => Nil
    }  */
  }
}
