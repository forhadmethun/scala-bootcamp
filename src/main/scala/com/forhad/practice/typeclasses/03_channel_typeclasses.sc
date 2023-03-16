
import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

/***
  * Pros: can be instance of any type,
  *       cleaner interface,
  *       several implementation possible
  */

trait ByteEncodable[A] {
  def encode(a: A): Array[Byte]
}

trait Channel {
  def write[A](obj: A, enc: ByteEncodable[A]): Unit
}

object StringEncodable extends ByteEncodable[String] {
  override def encode(a: String) = a.getBytes
}

object IntEncodable extends ByteEncodable[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(n)
    bb.array()
  }
}

object FileChannel extends Channel {
  override def write[A](obj: A, enc: ByteEncodable[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)
    Using(new FileOutputStream("/Users/forhad/Documents/Programming/myself/learning/evolution-bootcamp/scala-bootcamp/src/main/resources/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

FileChannel.write[Int](4, IntEncodable)
