import java.nio.ByteBuffer
import scala.util.Try

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
trait ByteDecoder[A] {
  def decode(a: Array[Byte]): Option[A]
}

object ByteEncoder {
  implicit val intEncoder: ByteEncoder[Int] = instance[Int](a => {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(a)
    bb.array()
  })

  implicit val stringEncoder: ByteEncoder[String] = instance[String](a => a.getBytes)

  implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = new ByteEncoder[Option[A]] {
    override def encode(a: Option[A]) = a match {
      case Some(value) => encA.encode(value)
      case None => Array[Byte]()
    }
  }

  def apply[A](implicit enc: ByteEncoder[A]): ByteEncoder[A] = enc
  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = new ByteEncoder[A] {
    override def encode(a: A): Array[Byte] = f(a)
  }
}

//ByteEncoder[Option[Int]].encode(Option(100))
