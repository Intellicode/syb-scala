package Syb3;
object syb3 { 
  trait Data[A] {
    
    trait ForallBC[W[_]] {
         def apply[B,C](w:W[B=>C], x : B)  : W[C]
         
    }
    
    trait ForallG[W[_]] {
        def apply[G](g :G) : W[G]
    }
    /**
     gfoldl :: Proxy ctx
            -> (forall b c.  w (b -> c) -> b -> w c)
            -> (forall g. g -> w g)
            -> a -> w a
    */
    
    def gfold[W[_]](k: ForallBC[W], z :ForallG[W], a:A):W[A]
  }
}
