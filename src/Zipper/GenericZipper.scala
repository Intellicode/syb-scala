package Zipper;

import Syb3.syb3._
import Syb3.Sample._

object test {

//  trait Data[A]

  abstract class Left[expects]

  /*
  trait ForallBC[W[_]] {
           def apply[B,C](w:W[B=>C], x : B) (implicit dt : Data[ctx,B]) : W[C]
      }

  {def apply[b](x : b)(implicit dt : Data[ctx,b]) : r} => a => List[r]
  */

  case class LeftUnit[EXPECTS](expects : EXPECTS) extends Left[EXPECTS] 

/*
 trait ForallG[W[_]] {
      def apply[G](g :G) : W[G]
    }
*/
  //case class LeftCons[B : Data, EXPECTS](f : Left[B => EXPECTS], b : B) extends Left[EXPECTS]
  case class LeftCons[B, EXPECTS](f : Left[B => EXPECTS], b : B) extends Left[EXPECTS]

  abstract class Right[provides, parent]
  case class RightUnit[parent,provides] () extends Right[provides, parent]
  //case class RightCons[provides, parent, A, T, B : Data](b : B, f : Right[A,T]) extends Right[provides, parent]

  abstract class Context[hole,root]
  case class CtxtUnit[hole, root] () extends Context[hole,root]
  //case class ConsCtxt[hole, root, parent : Data, rights] (left : Left[hole => rights], right : Right[rights, parent], ctxt : Context[parent, root])
  
  //context bound, implicit
  abstract class Zipper [root]

  case class ZipperC[X:Data,root] (h : X, ctxt : Context[X,root]) extends Zipper[root]
  //case class ZipperC[X,root] (h : Data[X], ctxt : Context[Data[X],root]) extends Zipper[root]

  //case class ZipperC[root, HOLE] (h : hole, ctxt : Context[hole, root])(implicit hole : Data[HOLE]) extends Zipper[root, hole]

  //def toZipper [X, root] (hole : Data[X]) : Zipper [root] = ZipperC(hole,CtxtUnit[Data[X],root]())
  def toZipper [X:Data, root] (hole : X)  : Zipper [root] = ZipperC(hole,CtxtUnit[X,root]()) (implicitly[Data[X]])
  //def toZipper [root, data : Data] (hole : data): Zipper [root, data] = ZipperC(hole, CtxtUnit()) 
  //def toZipper [root, HOLE] (h : Data[HOLE])(implicit hole : Data[HOLE]): Zipper [root, HOLE] = ZipperC(h, CtxtUnit())(hole)
  
  //def toLeft [X] (a : Data[X]) : Left[X] = Nothing
  
  

  def toLeft [EXPECTS,X] (a : X) (implicit data : Data[X]) : Left[X] = {
    def leftUnit = new ForallG[Left] {
      def apply[G](g : G) : Left[G] = LeftUnit(g)
    }
    def leftCons = new ForallBC[Left] {
      def apply[B,C](w:Left[B=>C], x : B)  : Left[C] = LeftCons(w, x)
    }
    data.gfold(leftCons, leftUnit, a)
  }


  //trait List[T]
  //case class Nil [A] () extends List[A]
  //case class Cons [A] (x : A, xs : List[A]) extends List[A]

  //val t = Nil()
  //val t2 = Cons(0, Nil ())
  
  //val h = toZipper(E("Tom",1))
  val h = toZipper(1 :: Nil)
  
  //def curryCons [T] : T => List[T] => List[T] = x => xs => Cons(x, xs)
  //def leftCons [EXPECTS,X] : Left[X => EXPECTS] => X => Left[EXPECTS] = f => x => LeftCons(f, x)

  //val j = LeftUnit(Nil())
  //val k = LeftUnit(curryCons)
  //val z = leftCons (LeftUnit(curryCons[Int])) (0)

  val k = toLeft (1 :: Nil)

}
