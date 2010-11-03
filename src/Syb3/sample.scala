package Syb3
import syb3._
object Sample {
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
							
  abstract case class DataDepartment[ctx[_]]() extends Data[ctx,Department] {
  	 self : ctx[Department] =>
  }
  
  abstract case class DataEmployee[ctx[_]]() extends Data[ctx,Employee] {
  	 self : ctx[Employee] =>
  	 
  	 
  }
  
}