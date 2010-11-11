package Syb3
import syb3._

object Sample {
	trait EmptyType
	case class Empty()() extends EmptyType
	case class Simple(a:Int,b:Int) extends EmptyType
	//case class
	
	trait Department {}
	case class D (manager:Manager,employees:List[Employee]) extends Department
	trait Employee {}
	case class E (name:Name, salary:Salary) extends Employee
	type Salary = Double
	type Manager = Employee
	type Name = String
	
	val sampleDepartment = D( E("Agamemnon",5000.0), 
	                          List( E("Menelaus",3000.0),
							        E("Achilles",2000.0),
							        E("Odysseus",3000.0)
							))
	
	
/*						
	trait DataEmptyType extends Data[EmptyType] {
		 def gfold[W[_]](k: ForallBC[W], z :ForallG[W], a:EmptyType):W[EmptyType] = {
  	 		a match {
  	 			case Empty() => z(Empty())
  	 			case Simple(a,b) => {
  	 				def currySimple:Int=>Int=> EmptyType = a=>b=>Simple(a,b)
  	 					k(k(z(currySimple),a),b) 
  	 				} 
  	 		} 	
  	 	}
	}*/		
				
	/*trait DataDepartment extends Data[Department] {
  	 	 def gfold[U >: Department,W[_]](k: ForallBC[W], z: ForallG[W], a:U):W[U] = {
  	 		a match {
  	 			case D(manager, employees) => {
  	 					def curryD:Manager => List[Employee] => Department = manager=>employees=>D(manager, employees)
  	 					k(k(z(curryD),manager),employees)
  	 				}
  	 		} 	
  	 	}
  	}*/
  	
  	trait DataEmployee extends Data[Employee] {
  		//def gfold[U >: A, W[_]](k: ForallBC[W], z :ForallG[W], a:U):W[U]
  	 	 def gfold[W[_]](k: ForallBC[W], z: ForallG[W], a:Employee):W[Employee] = {
  	 		a match {
  	 			case E(name, salary) => {
  	 					def curryE:Name => Salary => Employee = name=>salary=>E(name, salary):Employee
  	 					k(k(z(curryE),name),salary)
  	 				}
  	 		} 	
  	 	}
 	}

   /* trait DataList[+T] extends Data[List[T]] {
     // def gfold[U >: A, W[_]](k: ForallBC[W], z :ForallG[W], a:U):W[U]
      def gfold[U >: T,W[_]](k: ForallBC[W], z: ForallG[W], a:U):W[U] = {
        a match {
          case Nil     => z(Nil)
          case x :: xs => {
            def curryCons : T => U => U = x => xs => (x :: xs)
          //  k(k(z(curryCons),x),xs)
           z(Nil)
          }
        } 
      }
    }*/

//	implicit object dataEmptyType extends DataEmptyType
//	implicit object dataDepartment extends DataDepartment
	implicit object dataEmployee extends DataEmployee
	//implicit object dataList extends DataList[Int]
}
