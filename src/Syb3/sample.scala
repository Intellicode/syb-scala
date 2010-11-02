package syb3;
object Sample {
	trait Dept {}
	case class D (manager:Manager,employees:List[Employee]) extends Dept
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
}