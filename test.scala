trait Generic  [G[_]] {
    def unit :G[Unit ]
    def int :G[Int ]
    def char :G[Char ]
    def plus [a,b] :G[a]=>G[b]=>G[Either [a,b] ]
    def prod [a,b] :G[a]=>G[b]=>G[(a,b)]
    def view[a,b] : Iso[b,a]=>(=>G[a])=>G[b]
}
  
trait Iso[A,B] {
    def from : A => B
    def to   : B => A
}

def Iso[a,b] = (f : a => b) => (g : b => a) => new Iso[a,b] {def from = f; def to = g}

def isoId[A] = new Iso[A,A] {
    def from = x => x
    def to   = x => x
}

trait Trait[A] {
    def accept [g[_] :Generic ]  : g[A ]
}


implicit def RUnit = new Trait[Unit ] {
    def accept [g[_]:Generic ] = implicitly[Generic[g]].unit
  }

trait TypeConstructor {type a; type b}

trait Bifunctor[s <: Bifunctor[s]] extends TypeConstructor {
    def bimap[c, d](f :a=>c, g :b=>d):s{type a=c; type b=d}
}