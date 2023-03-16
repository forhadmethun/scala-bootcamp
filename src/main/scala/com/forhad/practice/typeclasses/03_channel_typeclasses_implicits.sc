
import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

/***
  * Pros: can be instance of any type
  *       cleaner interface
  *       several implementation possible
  */


/***
  * Common situation: for each type A, single byte encoder, couple of instance for specific use case
  *
  * Goal: use main instance by default, provide different instance for specific use case
  *
  */

//object ByteEncoder {
//  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
//  def instance[A](f: A => Array[Byte]): ByteEncoder[A]
//}

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
object ByteEncoder {
  implicit object StringEncoder$ extends ByteEncoder[String] {
    override def encode(a: String) = a.getBytes
  }

  implicit object IntEncoder$ extends ByteEncoder[Int] {
    override def encode(n: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(n)
      bb.array()
    }
  }
}

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}

 object Rot3StringEncoder extends ByteEncoder[String] {
  override def encode(a: String) = a.getBytes.map(a => (a+ 3).toByte)
}


object FileChannel extends Channel {
  override def write[A](obj: A) (implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)
    Using(new FileOutputStream("/Users/forhad/Documents/Programming/myself/learning/evolution-bootcamp/scala-bootcamp/src/main/resources/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

FileChannel.write[String]("hello")
