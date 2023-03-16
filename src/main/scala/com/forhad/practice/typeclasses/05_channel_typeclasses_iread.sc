import scala.util.Try

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

trait ByteDecoder[A] {
  def decode(a: Array[Byte]): Option[A]
}

object ByteDecoder {
  implicit val stringDecoder: ByteDecoder[String] = instance[String](a => Try(new String(a)).toOption)

  def apply[A](implicit dec: ByteDecoder[A]): ByteDecoder[A] = dec
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
    override def decode(a: Array[Byte]): Option[A] = f(a)
  }
}

trait channel {
  def write[A](a: A) (implicit enc: ByteEncoder[A]): Unit
  def read[A]()(implicit dec: ByteDecoder[A]): A
}

ByteDecoder[String].decode(Array(107, 104, 111, 111, 114))
