====================================================================================
SYB with Class in Scala 
====================================================================================

SYB with Class is a SYB port in Scala.

====================================================================================
Goals
====================================================================================
 - Write generic code only for the 'matching' GenericX trait
 - Write an instance for FRep and automatically have access to FRep2 and FRep3
 - Improve the collect function
 - Solve partial type  application
   - http://michid.wordpress.com/2008/07/30/meta-programming-with-scala-part-ii-multiplication/#comment-69
   - http://michid.wordpress.com/2008/08/27/meta-programming-with-scala-part-iii-partial-function-application/
   for instance this is not possible:
   case class Collect[A,B] (selCollect:A=>List[B])
   trait CollectG extends Generic[Collect] {
      //def unit = Collect
   }

   or

   case class Collect[A,B] (selCollect:A=>List[B])
   trait CollectG[B,A] extends Generic[Collect[B,A]] {
     //def unit = Collect
   }

====================================================================================
Usage
====================================================================================

====================================================================================
Todo
====================================================================================



- Read Generic Programming in Scala
  - Scala topics
    - Traits
    - implicits
    - case classes
- Read Scrap Your Boilerplate with Class
- 
