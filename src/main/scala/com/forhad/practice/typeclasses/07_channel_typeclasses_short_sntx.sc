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

  def apply[A](implicit enc: ByteEncoder[A]): ByteEncoder[A] = enc
  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = new ByteEncoder[A] {
    override def encode(a: A): Array[Byte] = f(a)
  }
}

object ByteDecoder {
  implicit val stringDecoder: ByteDecoder[String] = instance[String](a => Try(new String(a)).toOption)

  def apply[A](implicit dec: ByteDecoder[A]): ByteDecoder[A] = dec
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
    override def decode(a: Array[Byte]): Option[A] = f(a)
  }
}


implicit class ByteEncodeOps[A](a: A) {
  def encode(implicit enc: ByteEncoder[A]): Array[Byte] = enc.encode(a)
}

implicit class ByteDecodeOps[A](a: Array[Byte]) {
  def decode(implicit dec: ByteDecoder[A]): Option[A] = dec.decode(a)
}

5.encode(ByteEncoder.intEncoder)
//"hello".encode
("hello".encode(ByteEncoder.stringEncoder)).decode(ByteDecoder.stringDecoder)
