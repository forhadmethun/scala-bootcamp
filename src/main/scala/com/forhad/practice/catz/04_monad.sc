import cats._
import cats.implicits._

sealed trait MOption[+A] {
  def flatMap[A, B](f: A => MOption[B]): MOption[B] =
    this match {
      case MSome(a) => f(a)
      case MNone => MNone
    }
}

case class MSome[+A](a: A) extends MOption[A]

case object MNone extends MOption[Nothing]

//MNone.flatMap((x: Int) => MSome(x + 1)) === MNone
//MSome[Int](3).flatMap(x => MSome(x + 1)) === MSome(4)
//MSome(3).flatMap(x => MNone)
