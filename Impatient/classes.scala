object Classes {
  def main(args: Array[String]) = {

    // guard the increment() method
    class Counter {
      var value = 0
      def increment() = {
        if (!(value + 1 >= Int.MaxValue))
          value += 1
      }

      def current = value
    }

    val c = new Counter
    c.value = Int.MaxValue - 1
    println(c.current)
    c.increment()
    println(c.current)


    // Write a class BankAccount with methods deposit and withdraw, and a read-only property balance.
    class BankAccount (var money: Int) {
      def balance = money // read-only getter

      def deposit(amount: Int): Unit = {
        money += amount
      }

      def withdraw(amount: Int): Unit = {
        money -= amount
      }
    }

    val ba = new BankAccount(100)
    println(ba.balance)
    ba.deposit(150)
    println(ba.balance)
    ba.withdraw(39)
    println(ba.balance)

    // Write a class Time with read-only properties hours and minutes and a
    // method before(other: Time): Boolean that checks whether this time comes
    // before the other. A Time object should be constructed as new Time(hrs, min),
    // where hrs is in military time format (between 0 and 23).
    class Time() {
      private var hrs = 0
      private var min = 0

      def this(hrs: Int, min: Int) {
        this()
        if (hrs > -1 && hrs < 24 && min >= 0 && min <= 60) {
          this.hrs = hrs
          this.min = min
        }
      }

      def before(other: Time): Boolean = {
        if (this.hrs < other.hrs) true
        else if (this.hrs == other.hrs) this.min < other.min
        else false
      }
    }

    val t1 = new Time(12, 12)
    val t2 = new Time(25, 14)
    println(t1.before(t2))
    val t3 = new Time(11, 12)
    println(t1.before(t3))

    // In the Person class of Section 5.1, “Simple Classes and Parameterless
    // Methods,” on page 51, provide a primary constructor that turns negative ages
    // to 0.

    class Person (var age: Int){
      this.age = if (age < 0) 0 else age
    }

    val person = new Person(-3)
    println(person.age)
    val person2 = new Person(4)
    println(person2.age)

    // Make a class Car with read-only properties for manufacturer, model name,
    // and model year, and a read-write property for the license plate. Supply four
    // constructors. All require the manufacturer and model name. Optionally, model
    // year and license plate can also be specified in the constructor. If not, the
    // model year is set to -1 and the license plate to the empty string. Which
    // constructor are you choosing as the primary constructor? Why?

    class Car (val manufacturer: String,
               val modelName: String,
               val modelYear: Int = -1,
               val licensePlate: String = "") {
    }

    val car = new Car("mazda", "3", 2007, "japan")


    class Employee(val name: String, var salary: Double) {
      def this() {this("John Q. Public", 0.0)}
    }

    val employee = new Employee("eric", 130000)
    println(employee.name)
    println(employee.salary)

    class NewEmployee(val name: String = "John Q. Public",
                      var salary: Double = 0.0) {
    }
    val new_employee = new NewEmployee()
    println(new_employee.name)
    println(new_employee.salary)

    val new_employee2 = new NewEmployee("eric", 100)
    println(new_employee2.name)
    println(new_employee2.salary)

  }
}
