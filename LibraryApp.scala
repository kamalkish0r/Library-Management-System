import scala.io.StdIn.readLine

object LibraryApp {
  def main(args: Array[String]): Unit = {

    val library = Library()

    var continue = true
    println("Welcome to Library Management System!")
    while (continue) {
      println(
        """


Please select an option from following menu: 
 -----------------------------------------------------------------
| 1.  Add a customer             |   2.  Remove a customer        |
| 3.  Add a student              |   4.  Remove a student         |
| 5.  Show customer details      |   6.  Show student details     |
| 7.  Add a book                 |   8.  Update book quantity     |
| 9.  Search a book              |   10. Show all books           |
| 11. Check Out a book           |   12. Check In a book          |
| 13. Show all customers         |   14. Show all students        |
| 15. Deposit Money              |   16. Exit                     |
 -----------------------------------------------------------------

"""
      )

      try {
        var choice = readLine(
          "Enter number corresponding to your choice: "
        ).toInt

        choice match {
          case 1 =>
            var name = readLine("Enter customer name: ")
            var balance = -1
            while (balance < 0) {
              balance = readLine(
                "Enter initial account balance(>=0): "
              ).toInt
              if (balance < 0)
                println("Account balance can not be negative.")
            }

            library.addCustomer(name, balance)
          case 2 =>
            var customerId = readLine(
              "Enter Id of customer to be removed: "
            ).toInt
            library.removeCustomer(id = customerId)
          case 3 =>
            var name = readLine("Enter name of student: ")
            var rollNo = readLine("Enter Roll.No of student: ")
            var batch = readLine("Enter batch of student: ")
            library.addStudent(name, rollNo, batch)
          case 4 =>
            var studentId = readLine(
              "Enter Roll.No of student to be removed : "
            )
            library.removeStudent(rollNo = studentId)
          case 5 =>
            var customerId = readLine("Enter Id of customer: ").toInt
            library.showCustomerDetails(id = customerId)
          case 6 =>
            var studentId = readLine("Enter Roll.No of student: ")
            library.showStudentDetails(rollNo = studentId)
          case 7 =>
            var isbn: String = readLine("Enter ISBN of book: ")
            var title = readLine("Enter title of Book: ")
            var author = readLine("Enter author name: ")
            var quantity: Int = -1
            while (quantity < 0) {
              quantity = readLine("Enter quantity of book(>0): ").toInt
              if (quantity <= 0)
                println("Quantity must be greater than 0.")
            }
            var rentPrice: Double = -1
            while (rentPrice < 0) {
              rentPrice = readLine("Enter rent price of book(>=0): ").toDouble
              if (rentPrice < 0)
                println("Rent price must be >= 0.")
            }

            library.addBook(
              title = title,
              author = author,
              isbn = isbn,
              price = rentPrice,
              quantity = quantity
            )
          case 8 =>
            var isbn = readLine("Enter ISBN of book: ")
            var quantity = readLine("Enter quantity to be added: ").toInt
            library.updateBookQuantity(isbn = isbn, quantity = quantity)
          case 9 =>
            var query = readLine(
              "Enter title or author or isbn to search a book: "
            )
            library.searchBook(Option(query))
          case 10 =>
            library.showAllBooks()
          case 11 =>
            var userType = readLine(
              "Enter user type - Customer(C)/Student(S): "
            )
            var userId = readLine(
              "Enter Id of user.(For student => Roll.No, For customer => customerId): "
            )
            var isbn = readLine("Enter ISBN of book to check out: ")
            library.checkoutBook(userType, userId, isbn)
          case 12 =>
            var userType = readLine(
              "Enter user type - Customer(C)/Student(S): "
            )
            var userId = readLine(
              "Enter Id of user.(For student => Roll.No, For customer => customerId): "
            )
            var isbn = readLine("Enter ISBN of book to check In: ")
            library.checkinBook(userType, userId, isbn)
          case 13 =>
            library.showAllCustomers()
          case 14 =>
            library.showAllStudents()
          case 15 =>
            try {
              var customerId = readLine("Enter customerId: ").toInt
              try {
                var amount = readLine("Enter amount: ").toDouble
                library.depositMoney(id = customerId, amount = amount)
              } catch {
                case _ =>
                  println("Invalid Amount.")
              }
            } catch {
              case _ =>
                println("Invalid customer Id.")
            }
          case 16 =>
            println("Thank You for using Library Management System!")
            continue = false
          case _ =>
            println(
              "Invalid choice! Please choose an options from given list only."
            )
        }
      } catch {
        case a: NumberFormatException =>
          println(s"NumberFormatException occured. Try again!")
        case _ =>
          println(
            "Invalid choice! Please choose an options from given list only."
          )
      }
    }
  }
}
