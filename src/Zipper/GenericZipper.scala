package Zipper;

import Syb3.syb3._
//import Syb3.Sample._

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

  case class ZipperC[X,root] (h : X, ctxt : Context[X,root]) extends Zipper[root]
  //case class ZipperC[X,root] (h : Data[X], ctxt : Context[Data[X],root]) extends Zipper[root]

  //case class ZipperC[root, HOLE] (h : hole, ctxt : Context[hole, root])(implicit hole : Data[HOLE]) extends Zipper[root, hole]

  //def toZipper [X, root] (hole : Data[X]) : Zipper [root] = ZipperC(hole,CtxtUnit[Data[X],root]())
  def toZipper [X, root] (hole : X) : Zipper [root] = ZipperC(hole,CtxtUnit[X,root]())
  //def toZipper [root, data : Data] (hole : data): Zipper [root, data] = ZipperC(hole, CtxtUnit()) 
  //def toZipper [root, HOLE] (h : Data[HOLE])(implicit hole : Data[HOLE]): Zipper [root, HOLE] = ZipperC(h, CtxtUnit())(hole)
  
  //def toLeft [X] (a : Data[X]) : Left[X] = Nothing
  /*
  def toLeft [EXPECTS,X] (a : Data[X]) : Left[X] = {
    def leftCons : Left[X => EXPECTS] => X => Left[EXPECTS] = f => x => LeftCons(f, x)
    def leftUnit : EXPECTS => Left[EXPECTS] = x => LeftUnit(x)
    a.gfold _ leftCons leftUnit a
  }
  */
/*
  trait List[T] extends Data[T] {
    def gfold[W[_]](k: ForallBC[W], z: ForallG[W], a:List[T]):W[List[T]] = {
        a match {
          //case Nil => z(Nil())
          case Cons (x, xs) => {
            def curryCons : T => List[T] => List[T] = x => xs => Cons(x, xs)
            k(k(z(curryCons),x),xs)
          }
        } 
    }
  }
*/

  trait List[T]

  case class Nil [A] () extends List[A]
  case class Cons [A] (x : A, xs : List[A]) extends List[A]

  val t = Nil()
  val t2 = Cons(0, Nil ())
  
  //val h = toZipper(E("Tom",1))
  val h = toZipper(Nil())
  
  def curryCons [T] : T => List[T] => List[T] = x => xs => Cons(x, xs)
  def leftCons [EXPECTS,X] : Left[X => EXPECTS] => X => Left[EXPECTS] = f => x => LeftCons(f, x)

  val j = LeftUnit(Nil())
  val k = LeftUnit(curryCons)
  val z = leftCons (LeftUnit(curryCons)) (0)
  
}