import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(a: String) = a.getBytes
  }

  implicit val stringByteEncoder: ByteEncoder[String] = instance[String](_.getBytes)

  def summon[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = new ByteEncoder[A] {
    override def encode(a: A) = f(a)
  }
}

//implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
//  override def encode(a: String) = a.getBytes.map(b => (b+3).toByte)
//}

implicit val rot3StringByteEncoder: ByteEncoder[String] =
  ByteEncoder.instance[String](s => s.getBytes.map(b => (b+3).toByte))


//implicitly[ByteEncoder[String]]
//ByteEncoder.StringByteEncoder
//  ByteEncoder.summon[String]
ByteEncoder[String]
  .encode("hello")
