package Zipper;

import Syb3.syb3._
import Syb3.Sample._

object test {

  abstract class Left[expects]
  case class LeftUnit[EXPECTS](expects : EXPECTS) extends Left[EXPECTS] 
  case class LeftCons[B, EXPECTS](f : Left[B => EXPECTS], b : B) extends Left[EXPECTS]

  abstract class Right[+provides, parent]
  case class RightUnit[provides,parent] () extends Right[provides, parent]
  case class RightCons[A, T, B](b : B, f : Right[A,T]) extends Right[B => A, T]

  abstract class Context[hole,root]
  case class CtxtUnit[hole, root] () extends Context[hole,root]
  case class CtxtCons[hole, root, parent, rights] (left : Left[hole => rights], right : Right[rights, parent], ctxt : Context[parent, root]) extends Context[hole,root]
  
  //context bound, implicit
  abstract class Zipper [root]

  case class ZipperC[X,root] (h : X, ctxt : Context[X,root]) extends Zipper[root]

  //case class ZipperC[X,root] (h : Data[X], ctxt : Context[Data[X],root]) extends Zipper[root]

  //case class ZipperC[root, HOLE] (h : hole, ctxt : Context[hole, root]) extends Zipper[root, hole]

  //def toZipper [X, root] (hole : Data[X]) : Zipper [root] = ZipperC(hole,CtxtUnit[Data[X],root]())
  //def toZipper [X:Data, root] (hole : X)  : Zipper [root] = ZipperC(hole,CtxtUnit[X,root]()) (implicitly[Data[X]])
  def toZipper [X : Data] (hole : X): Zipper [X] = ZipperC(hole, CtxtUnit()) 
  //def toZipper [root, HOLE] (h : Data[HOLE])(implicit hole : Data[HOLE]): Zipper [root, HOLE] = ZipperC(h, CtxtUnit())(hole)
  
  //def toLeft [X] (a : Data[X]) : Left[X] = Nothing
  
  /*
  def toLeft [X : Data] (a : X) : Left[X] = {
    def leftUnit = new ForallG[Left] {
      def apply[G](g : G) : Left[G] = LeftUnit(g)
    }
    def leftCons = new ForallBC[Left] {
      def apply[B,C](w:Left[B=>C], x : B)  : Left[C] = LeftCons(w, x)
    }
    (implicitly[Data[X]]).gfold(leftCons, leftUnit, a)
  }
*/

  def toLeft [X] (a : X) (implicit d : Data[X]) : Left[X] = {
    def leftUnit = new ForallG[Left] {
      def apply[G](g : G) : Left[G] = LeftUnit(g)
    }
    def leftCons = new ForallBC[Left] {
      def apply[B,C](w:Left[B=>C], x : B)  : Left[C] = LeftCons(w, x)
    }
    d.gfold(leftCons, leftUnit, a)
  }

  def fromLeft [R] : Left[R] => R = l => {
    l match {
      case LeftUnit(a) => a
      case LeftCons(f, b) => (fromLeft (f)) (b)
    }
  }

/*
left  :: Zipper a -> Maybe (Zipper a)
left (Zipper _ CtxtNull) = Nothing
left (Zipper _ (CtxtCons (LeftUnit _) _ _)) = Nothing
left (Zipper h (CtxtCons (LeftCons l h') r c)) =
  Just (Zipper h' (CtxtCons l (RightCons h r) c))
*/

  def left[A] (z : Zipper[A]) : Option[Zipper[A]] = {
    z match {
      case ZipperC(_, CtxtUnit()) => None
      case ZipperC(_, CtxtCons(LeftUnit(_), _, _)) => None
      case ZipperC(h1, CtxtCons(LeftCons(l,h2),r,c)) =>
        Some(ZipperC(h2, (CtxtCons(l, RightCons(h1,r), c))))
    }
  }

/*
-- | Move right.  Returns 'Nothing' iff already at rightmost sibling.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ CtxtNull) = Nothing
right (Zipper _ (CtxtCons _ RightNull _)) = Nothing
right (Zipper h (CtxtCons l (RightCons h' r) c)) =
  Just (Zipper h' (CtxtCons (LeftCons l h) r c))
*/

   def right[A] (z:Zipper[A]) : Option[Zipper[A]] = {
    z match {
      case ZipperC(_, CtxtUnit()) => None
      case ZipperC(_, CtxtCons(_, RightUnit(), _)) => None
      case ZipperC(h1, CtxtCons(l, RightCons(h2, r), c)) => {
        Some(ZipperC(h2, CtxtCons(LeftCons(l,h1), r, c)))
      }
    }
  }

/*
case class LeftCons[B, EXPECTS](f : Left[B => EXPECTS], b : B) extends Left[EXPECTS]
        def mkleft[B,EXPECTS](l:LeftCons[B, EXPECTS]):Left[EXPECTS] = l

  def right[A] (z:Zipper[A]) : Option[Zipper[A]] = {
    z match {
      case ZipperC(_, CtxtUnit()) => None
      case ZipperC(_, CtxtCons(_, RightUnit(), _)) => None
      //case ZipperC(h1, CtxtCons(l, rightCons, c) ) => {
      case ZipperC(h1, CtxtCons(l, RightCons(h2, r), c)) => {
        //Some(ZipperC(h2, (CtxtCons(LeftCons(l,h1),r , c))))
        None
        //val leftCons = LeftCons(l, h1)
        //Some(ZipperC(h2, (CtxtCons(leftCons, r, c))))
            
            // because scala's type inferencer is too stupid to figure out that
            // a RightCons is a Right
            def mkrightCons[A,T,B](a:Right[_,_]):Right[B => A, T] = a.asInstanceOf[Right[B=>A,T]]
            
            //because scala's type inferencer is too stupid to figure out that leftcons is a left
            //def mkleft[B,EXPECTS](l:LeftCons[B, EXPECTS]):Left[EXPECTS] = l
            val r = mkrightCons(rightCons)
            r match{
               case  RightCons(h2, r)=> Some(ZipperC(h2, (CtxtCons(LeftCons(l,h1),r , c)))) 
               //case _ => None
            }
          }
    }
  }
*/

/*
down  :: Zipper a -> Maybe (Zipper a)
down (Zipper hole ctxt) =
  case toLeft hole of
    LeftUnit _ -> Nothing
    LeftCons l hole' ->
      Just (Zipper hole' (CtxtCons l RightNull ctxt))
*/

/*
  def down[A] (z : Zipper [A], d : Data[Any]) : Option[Zipper[A]] = {
    z match {
      case ZipperC(hole1,ctxt) => {
        toLeft(hole1)(d) match {
          case LeftUnit(_) => None
          case LeftCons(l, hole2) => 
            Some(ZipperC(hole2, CtxtCons(l, RightUnit(), ctxt)))
        }
      }
    }
  }
*/

  def down[X] (z : Zipper [X])(implicit d : Data[X]) : Option[Zipper[X]] = {
    z match {
      case ZipperC(hole1,ctxt) => {
        toLeft(hole1)(d) match {
          case LeftUnit(_) => None
          case LeftCons(l, hole2) => 
            Some(ZipperC(hole2, CtxtCons(l, RightUnit(), ctxt)))
        }
      }
    }
  }

  //trait List[T]
  //case class Nil [A] () extends List[A]
  //case class Cons [A] (x : A, xs : List[A]) extends List[A]

  //val t = Nil()
  //val t2 = Cons(0, Nil ())
  
  //val h = toZipper(E("Tom",1))
//  val h = down (toZipper(1 :: Nil))
  
  //def curryCons [T] : T => List[T] => List[T] = x => xs => Cons(x, xs)
  //def leftCons [EXPECTS,X] : Left[X => EXPECTS] => X => Left[EXPECTS] = f => x => LeftCons(f, x)
//RightCons[A, T, B](b : B, f : Right[A,T]) extends Right[B => A, T]
  //def rightCons[A,T,B] : B => Right[A,T] => Right[B => A, T] = b => f => RightCons(b, f)

  //val j = LeftUnit(Nil())
  //val k = LeftUnit(curryCons)
  //val l = leftCons (LeftUnit(curryCons[Int])) (0)
  //val r = rightCons (0,RightUnit())
  //val r = rightCons (  
  //val x = CtxtCons(l, r, CtxtUnit())
  //val z = LeftCons(LeftUnit(:: _), 0)

  //val k = toLeft (1 :: Nil)

}
