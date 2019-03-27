package priority

class Todo(val description:String, var priority:Int, private var dueDate: String = null) {
	def incrementPriority = priority = priority + 1
			def decrementPriority = priority = priority - 1
			def setDueDate(month:Int, day:Int, year:Int = 2019) ={
					dueDate = month + "/" + day+ "/" + year
	}
	override def toString() = {
			if(dueDate != null)
				"Description: " + description + " Priority: " + priority + " Due date: " + dueDate + "\n"
				else "Description: " + description + " Priority: " + priority + "\n"
	}
}
object Todo{
	def inOrderOfPriority(l:List[Todo]): List[Todo]= {
			if(l != Nil){
				var maxPriority = 0;
				for(element <- l){
					if(element.priority>maxPriority) maxPriority = element.priority
				}
				val rest = l.filter((t:Todo)=> t.priority != maxPriority)
						l.filter((t:Todo)=> t.priority == maxPriority)++inOrderOfPriority(rest)
			}
			else l
	}
	def topPriority(l:List[Todo]) = {
			var maxPriority = 0;
			for(element <- l){
				if(element.priority>maxPriority) maxPriority = element.priority
			}
			l.filter((t:Todo)=> t.priority == maxPriority)
	}
	def apply(d:String, p:Int) = new Todo(d, p)
			def apply(d:String, p:Int, da:String) = new Todo(d, p, da)
			def test{
		val phyMidterm = Todo("Physics Midterm", 10)
				phyMidterm.setDueDate(3, 27)
				val cs152Midterm = Todo("CS 152 Midterm", 10)
				cs152Midterm.setDueDate(3, 27, 2019)
				val vacation = Todo("Future vacation", 0)
				vacation.setDueDate(3, 20, 2020)
				val unknownHW = Todo("New HW", 2)
				val todoList = List(phyMidterm, cs152Midterm, vacation, unknownHW)
				println("FULL LIST: " + todoList)
				println("Priority: " + inOrderOfPriority(todoList))
				println("Top Priority: " + topPriority(todoList))
				phyMidterm.decrementPriority
				println("After decrementing phys midterm")
				println("Priority: " + inOrderOfPriority(todoList))
				println("Top Priority: " + topPriority(todoList))
	}
	object Tester extends App {
		Todo.test
	}
}
