import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)


object Account {
  implicit val toStringShow: Show[Account] = Show.fromToString

  object Instances {
    implicit val byOwnerAndBalance: Show[Account] = Show.show { account =>
      s"${account.owner} - $$${account.balance}"
    }

    implicit val prettyByOwner: Show[Account] = Show.show { account =>
      s"This account is owned by : ${account.owner}"
    }
  }
}

val acc1 = Account(1, "123-56", 1000, "Forhad")
val acc2 = Account(2, "123-56", 1500, "Hossain")

Account.toStringShow.show(acc1)

import Account.Instances.prettyByOwner
acc2.show
