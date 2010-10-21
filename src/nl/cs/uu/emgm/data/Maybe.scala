package nl.cs.uu.emgm.data;

import nl.cs.uu.emgm.EMGM._;

object Maybe {

/*
  trait Maybe [A] extends Iso[Maybe[A], Either [Unit, A]] {
    def from [A] = (m : Maybe [A]) => m match {
      case Nothing ()  => Left (())
      case Just (x) => Right (x)
    }
    def to [A] = (m : Either [Unit, A]) => m match {
      case Left (_) => Nothing [A]
      case Right (x) => Just (x)
    }
  }
*/
  trait Maybe [A]
  case class Nothing [A] () extends Maybe [A]
  case class Just [A] (a : A) extends Maybe [A]

  def from [A] = (m : Maybe [A]) => m match {
    case Nothing ()  => Left (())
    case Just (x) => Right (x)
  }

  def to [A] = (m : Either [Unit, A]) => m match {
    case Left (_) => Nothing [A]
    case Right (x) => Just (x)
  }

  def maybeIso [A] = 
    Iso[Maybe [A], Either [Unit, A]] (from) (to)
}
