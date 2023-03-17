import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit val universalEa: Eq[Account] = Eq.fromUniversalEquals

  implicit def orderById(implicit orderingLong: Order[Long]): Order[Account] =
    Order.from((a1, a2) => orderingLong.compare(a1.id, a2.id))

  object Instances {
    implicit def byIdEq(implicit eqLong: Eq[Long]): Eq[Account] =
      Eq.instance[Account]((ac1, ac2) => ac1.id == ac2.id)

    implicit def byIdEq2(implicit eqLong: Eq[Long]): Eq[Account] = Eq.by(_.id)

    implicit def byNumberEq(implicit eqString: Eq[String]): Eq[Account] = Eq.by(_.number)

    implicit val orderByNumber: Order[Account] = Order.by(account => account.number)

    implicit def orderByBalance(implicit orderDouble: Order[Double]): Order[Account] = Order.by(account => account.balance)
  }
}


val acc1 = Account(1, "123-56", 1000, "Forhad")
val acc2 = Account(2, "123-56", 1500, "Hossain")

Eq[Account].eqv(acc1, acc2)

implicit val eqToUse: Eq[Account] = Account.Instances.byNumberEq
acc1 === acc2


def sort[A](list: List[A])(implicit orderA: Order[A]) = {
  list.sorted(orderA.toOrdering)
}
Account.Instances.orderByNumber
implicit val orderByIdDesc: Order[Account] = Order.reverse(Account.orderById)
sort[Account](List(acc1, acc2))
