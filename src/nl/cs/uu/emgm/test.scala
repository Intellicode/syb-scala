trait Iso[A,B] {
    def from : A => B
    def to   : B => A
  }

  def Iso[a,b] = (f : a => b) => (g : b => a) => new Iso[a,b] {def from = f; def to = g} 

trait Generic  [G[_]] {
    def unit :G[Unit ]
    def int :G[Int ]
    def char :G[Char ]
    def plus [a,b] :G[a]=>G[b]=>G[Either [a,b] ]
    def prod [a,b] :G[a]=>G[b]=>G[(a,b)]
    def view[a,b] : Iso[b,a]=>(=>G[a])=>G[b]
  }

/**********************************************8
   * Rep
   */
  trait Rep[T ] {
    def accept [g[_] ] (implicit gen :Generic [g]) : g[T]
  }

  implicit def RUnit = new Rep[Unit ] {
    def accept [g[_] ] (implicit gen :Generic [g]) = gen.unit
  }

  implicit def RInt = new Rep[Int ] {
    def accept [g[_] ] (implicit gen :Generic [g]) = gen.int
  }

  implicit def RChar = new Rep[Char ] {
    def accept [g[_] ] (implicit gen :Generic [g]) = gen.char
  }

  implicit def RPlus [a,b] (implicit a :Rep[a],b :Rep[b]) = new Rep[Either [a,b] ] {
    def accept [g[_] ] (implicit gen :Generic [g]) =
      gen.plus (a.accept [g] (gen)) (b.accept [g] (gen))
  }

  implicit def RProd [a,b] (implicit a :Rep[a],b :Rep[b]) = new Rep[(a,b)] {
    def accept [g[_] ] (implicit gen :Generic [g]) =
      gen.prod (a.accept [g] (gen)) (b.accept [g] (gen))
  }

  /**********************************************8
   * FRep
   */
  trait FRep[F[_]] {
    def accept [a,g[_]] (ra : g[a]) (implicit gen : Generic [g]) : g [F[a]]
  }
 
case class Count[A] (count:A=>Int)
  trait CountG extends Generic[Count] {
    def unit = Count (x=>0)
    def int = Count (x=>0)
    def char = Count (x=>0)
    def plus [a,b] = a=>b=>Count (_.fold (a.count,b.count))
    def prod [a,b] = a=>b=>Count (x=>a.count (x._1)+b.count (x._2))
    def view[a,b] = iso=>a=>Count (x=>a.count (iso.from (x)))
  }
object Count extends CountG

  type RDirection =  Either [Unit, Either[Unit,Either[Unit,Unit]]]
  trait Direction
  case class North () extends Direction
  case class East () extends Direction
  case class West () extends Direction
  case class South () extends Direction


  def fromDirection  = (s :Direction)=>s match {
    case North() =>Left ({})
    case East() => Right(Left({}))
    case West() => Right(Right(Left({})))
    case South() => Right(Right((Right({}))))
  }

  def toDirection = (s :RDirection)=>s match {
    case Left(())  =>North()
    case Right(Left(()))  =>East()
    case Right(Right(Left(()))) => West()
    case Right(Right(Right(()))) => South()
  }

  def directionIso = Iso[Direction,RDirection ] (fromDirection) (toDirection)
  val assertIso = directionIso.to(directionIso.from(South())) == South()

implicit object countG extends CountG
  def gCount [a] (x : a) (implicit rep :Rep[a]) = rep.accept [Count ].count (x)