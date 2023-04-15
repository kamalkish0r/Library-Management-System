import scala.collection.mutable.{Map => MutableMap}

class Customer(
    val name: String,
    val id: Int,
    var accountBalance: Double = 0.0
) {
  var rentedBooks: List[String] = Nil
  override def toString(): String =
    s"$name, Balance: $accountBalance, Id: $id"
}

class Student(val name: String, val rollNo: String, val batch: String) {
  var rentedBooks: List[String] = Nil
  override def toString(): String =
    s"$name, Batch: $batch RollNo: $rollNo"
}

class Book(
    val title: String,
    val isbn: String,
    val author: String,
    val price: Double,
    var quantity: Int
) {
  override def toString: String =
    s"`$title` by `$author` (ISBN: $isbn Price: $price, Quantity: $quantity)"
}

class Library {
  private val customers: MutableMap[Int, Customer] = MutableMap.empty
  private val students: MutableMap[String, Student] = MutableMap.empty
  private val books: MutableMap[String, Book] = MutableMap.empty
  var nextCustomerId = 1

  def addCustomer(name: String, accountBalance: Double): Unit = {
    var id: Int = nextCustomerId
    nextCustomerId += 1
    val customer = new Customer(name, id, accountBalance)
    customers.put(id, customer)
    println(
      s"Customer [Name: $name, Id: $id, Balance: $accountBalance] added successfully."
    )
  }

  def removeCustomer(id: Int): Unit = {
    var customer = customers.get(id)
    customer match {
      case Some(c) =>
        customers.remove(id).nonEmpty
        println(s"Customer with Id: $id removed successfully.")
      case None =>
        println(s"No customer exists with Id: $id.")
    }
  }

  def addStudent(name: String, rollNo: String, batch: String): Unit = {
    var student = students.get(rollNo)
    student match {
      case Some(c) =>
        println(s"Student with Roll.No: $rollNo already exists!")
      case None =>
        val student = new Student(name, rollNo, batch)
        students.put(rollNo, student)
    }
  }

  def removeStudent(rollNo: String): Unit = {
    var student = students.get(rollNo)
    student match {
      case Some(c) =>
        students.remove(rollNo).nonEmpty
        println(s"Student with RollNo: $rollNo removed successfully.")
      case None =>
        println(s"No student exists with RollNo: $rollNo.")
    }
  }

  def depositMoney(id: Int, amount: Double): Unit = {
    if (amount <= 0) {
      println("Invalid amount! Amount must be > 0.")
      return
    }

    customers.get(id) match {
      case Some(customer) =>
        customer.accountBalance += amount
        println(
          s"Account balance updated. ${customer}"
        )
      case None =>
        println("Customer not found!")
    }
  }

  def showAllCustomers(): Unit = {
    println("All Customers:")
    if (customers.isEmpty) {
      println("No Customers Found!")
      return
    }

    customers.values.foreach(customer =>
      println(
        s"${customer.id}: ${customer.name}, Balance: ${customer.accountBalance}"
      )
    )
  }

  def showCustomerDetails(id: Int): Unit = {
    customers.get(id) match {
      case Some(customer) =>
        println("Customer Details:")
        println(customer)
        println("Books Rented:")
        if (customer.rentedBooks.isEmpty) {
          println("None")
          return
        }

        customer.rentedBooks.foreach(isbn =>
          println(s" - ${books.apply(isbn)}")
        )
      case None =>
        println(s"No customer with Id $id found.")
    }
  }

  def showStudentDetails(rollNo: String): Unit = {
    students.get(rollNo) match {
      case Some(student) =>
        println("Student Details:")
        println(student)
        // println(s"Name: ${student.name}")
        // println(s"Roll No: ${student.rollNo}")
        // println(s"Batch: ${student.batch}")
        println("Books Rented:")
        if (student.rentedBooks.isEmpty) println("None")
        else
          student.rentedBooks.foreach(isbn =>
            println(s" - ${books.apply(isbn)}")
          )
      case None =>
        println(s"No student found with Roll No: $rollNo")
    }
  }

  def showAllStudents(): Unit = {
    println("All Registered Students:")
    if (students.isEmpty) println("No Students Found!")
    else
      students.foreach(s =>
        println(s"${s._1} - ${s._2.name} - ${s._2.rollNo} - ${s._2.batch}")
      )
  }

  def addBook(
      title: String,
      isbn: String,
      author: String,
      price: Double,
      quantity: Int
  ): Unit = {
    val book = Book(
      title = title,
      isbn = isbn,
      author = author,
      price = price,
      quantity = quantity
    )
    books.put(isbn, book)
    println("Book added successfully!")
  }

  def updateBookQuantity(isbn: String, quantity: Int): Unit = {
    books.get(isbn) match {
      case Some(book) =>
        if (quantity > 0) {
          book.quantity = quantity
          println(
            s"Quantity of book '${book.title}' updated successfully!\n$book"
          )
        } else {
          println(
            "Invalid quantity. Quantity should be greater than 0."
          )
        }
      case None =>
        println(s"No book found with ISBN: $isbn")
    }
  }

  def searchBook(
      query: Option[String]
  ): Unit = {
    val filteredBooks = books.filter { case (id, b) =>
      query.forall(q =>
        b.title.toLowerCase.contains(q.toLowerCase) ||
          b.author.toLowerCase.contains(q.toLowerCase) ||
          b.isbn.toLowerCase.contains(q.toLowerCase)
      )
    }

    println("Search Result:")
    if (filteredBooks.isEmpty) println("None")
    else
      filteredBooks.foreach { case (id, book) =>
        // println(s"${b.title} - ${b.author} - ${b.isbn} - ${b.price}")
        println(book)
      }
  }

  def showAllBooks(): Unit = {
    println("All books : ")
    if (books.isEmpty) println("None")
    else
      books.foreach { case (id, book) =>
        // println(
        //   s"${b.title} - ${b.author} - ${b.isbn} - ${b.price} - quantity : ${b.quantity}"
        // )
        println(book)
      }
  }

  def checkoutBook(userType: String, id: String, isbn: String): Unit = {
    books.get(isbn) match {
      case Some(book) =>
        if (book.quantity <= 0) {
          println("Book is out of stock!")
          return
        }

        if (
          userType.toLowerCase() == "s" || userType.toLowerCase() == "student"
        ) {
          students.get(id) match {
            case Some(student) =>
              student.rentedBooks = student.rentedBooks :+ book.isbn
              book.quantity -= 1
              println(s"${student.name} has rented ${book.title}")
            case None =>
              println("Student not found!")
          }
        } else if (
          userType.toLowerCase() == "c" || userType.toLowerCase() == "customer"
        ) {
          customers.get(id.toInt) match {
            case Some(customer) =>
              if (customer.accountBalance < book.price) {
                println("Insufficiant Balance!")
                return
              }
              customer.accountBalance -= book.price
              customer.rentedBooks = customer.rentedBooks :+ book.isbn
              book.quantity -= 1
              println(s"${customer.name} has rented ${book.title}")
            case None =>
              println("Customer Not Found!")
          }
        } else {
          println("Invalid user type")
        }
      case None =>
        println("Book not found")
    }
  }

  def checkinBook(userType: String, id: String, isbn: String): Unit = {
    books.get(isbn) match {
      case Some(book) =>
        if (
          userType.toLowerCase() == "s" || userType.toLowerCase() == "student"
        ) {
          students.get(id) match {
            case Some(student) =>
              if (!student.rentedBooks.contains(book.isbn)) {
                println(s"${student.name} did not rent ${book.title}")
                return
              }
              student.rentedBooks = student.rentedBooks.filter(_ != book.isbn)
              book.quantity += 1
              println(s"${student.name} has returned ${book.title}")
            case None =>
              println("Student not found")
          }
        } else if (
          userType.toLowerCase() == "c" || userType.toLowerCase() == "customer"
        ) {
          customers.get(id.toInt) match {
            case Some(customer) =>
              if (!customer.rentedBooks.contains(book.isbn)) {
                println(s"${customer.name} did not rent ${book.title}")
                return
              }

              customer.rentedBooks = customer.rentedBooks.filter(_ != book.isbn)
              book.quantity += 1
              println(s"${customer.name} has returned ${book.title}")
            case None =>
              println("Customer not found")

          }
        } else {
          println("Invalid user type")
        }
      case None =>
        println("Book not found")
    }
  }
}
