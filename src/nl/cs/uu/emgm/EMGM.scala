package nl.cs.uu.emgm;
  
object EMGM {

  /**********************************************8
   * Iso
   */
  trait Iso[A,B] {
    def from : A => B
    def to   : B => A
  }

  def Iso[a,b] = (f : a => b) => (g : b => a) => new Iso[a,b] {def from = f; def to = g}

  def isoId[A] = new Iso[A,A] {
    def from = x => x
    def to   = x => x
  }

  /**********************************************8
   * Generics
   */
  trait Generic  [G[_]] {
    def unit :G[Unit ]
    def int :G[Int ]
    def char :G[Char ]
    def plus [a,b] :G[a]=>G[b]=>G[Either [a,b] ]
    def prod [a,b] :G[a]=>G[b]=>G[(a,b)]
    def view[a,b] : Iso[b,a]=>(=>G[a])=>G[b]
  }

  trait Generic2 [G[_,_]] {
    def unit :G[Unit, Unit ]
    def int :G[Int, Int ]
    def char :G[Char, Char ]
    def plus [a1,a2,b1,b2] :G[a1, a2]=>G[b1, b2]=>G[Either [a1,b1], Either [a2, b2] ]
    def prod [a1,a2,b1,b2] :G[a1, a2]=>G[b1, b2]=>G[(a1,b1),(a2,b2)]
    def view[a1,a2,b1,b2] : Iso[b1,a1]=>Iso[b2,a2]=>(=>G[a1, a2])=>G[b1 ,b2]
  }

  trait Generic3 [G[_,_,_] ] {

    def unit :G[Unit, Unit,Unit ]
    def int :G[Int, Int, Int ]
    def char :G[Char, Char, Char ]
    def plus [a1,a2,a3,b1,b2,b3] :G[a1, a2, a3]=>G[b1, b2, b3]=>G[Either [a1,b1], Either [a2, b2], Either [a3, b3] ]
    def prod [a1,a2,a3,b1,b2,b3] :G[a1, a2, a3]=>G[b1, b2, b3]=>G[(a1,b1),(a2,b2),(a3,b3)]
    def view[a1,a2,a3,b1,b2,b3] : Iso[b1,a1]=>Iso[b2,a2]=>Iso[b3,a3]=>(=>G[a1, a2, a3])=>G[b1 ,b2, b3]
  }

  /**********************************************8
   * Rep
   */
  trait Rep[T ] {
    def accept [g[_]:Generic  ] : g[T ]
  }

  implicit def RUnit = new Rep[Unit ] {
    def accept [g[_]:Generic ] = implicitly[Generic[g]].unit
  }

  implicit def RInt = new Rep[Int ] {
    def accept [g[_]:Generic ] = implicitly[Generic[g]].int
  }

  implicit def RChar = new Rep[Char ] {
    def accept [g[_]:Generic ] = implicitly[Generic[g]].char
  }

  implicit def RPlus [a:Rep,b:Rep]  = new Rep[Either [a,b] ] {
    def accept [g[_]:Generic ] =
      implicitly[Generic[g]].plus ( implicitly[Rep[a]].accept [g] (implicitly[Generic[g]]))  ( implicitly[Rep[b]].accept [g] (implicitly[Generic[g]]))
  }

  implicit def RProd [a:Rep,b:Rep] = new Rep[(a,b)] {
    def accept [g[_]:Generic ]  =
      implicitly[Generic[g]].prod ( implicitly[Rep[a]].accept [g] (implicitly[Generic[g]])) (implicitly[Rep[b]].accept [g] (implicitly[Generic[g]]))
  }

  /**********************************************8
   * FRep
   */
  trait FRep[F[_]] {
    def accept [a,g[_]:Generic] (ra : g[a]) : g [F[a]]
  }

  /**********************************************8
   * FRep2
   */
  trait FRep2[F[_]] {
    //not sure if this is the correct type since F is not bound to the types of g
    def accept [a,b,g[_,_]:Generic2] (rab : g[a,b]) : g [F[a],F[b]]
  }
  
  /**
   * BiFRep 2
   */
  trait BiFRep2[F[_,_]] {
    def accept [a,b,c,d,g[_,_]:Generic2] (rab : g[a,b]) (rcd : g[c,d]) : g [F[a,c],F[b,d]]
  }

  /** 
   * FRep3
   */
  trait FRep3[F[_]] {
    def accept [a,b,c,g[_,_,_]:Generic3] (rabc : g[a,b,c]): g[F[a],F[b],F[c]]
  }
  
  implicit def bla = 0;
  case class Count [A] (count :A=>Int) (implicit bla:Int)
  // type G1[a] = Generic3[a,(),()]   ;
  trait CountG extends Generic[Count] {
    def unit = Count (x=>0)
    def int = Count (x=>0)
    def char = Count (x=>0)
    def plus [a,b] = a=>b=>Count (_.fold (a.count,b.count))
    def prod [a,b] = a=>b=>Count (x=>a.count (x._1)+b.count (x._2))
    def view[a,b] = iso=>a=>Count (x=>a.count (iso.from (x)))
  }
}
