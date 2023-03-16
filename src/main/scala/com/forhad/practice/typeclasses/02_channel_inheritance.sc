import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

/***
  * Pros: unique responsibility, easy to test, unhandled type -> compile error
  * Cons: How do we extend Int? only one implementation, overload interface
  */

trait Channel {
  def write(obj: ByteEncodable): Unit
}

trait ByteEncodable {
  def encode(): Array[Byte]
}

case class FullName(firstName: String, lastName: String) extends ByteEncodable {
  override def encode() = firstName.getBytes ++ lastName.getBytes
}

object FileChannel extends Channel {
  override def write(obj: ByteEncodable): Unit = {
    val bytes:Array[Byte] = obj.encode()
    Using(new FileOutputStream("/Users/forhad/Documents/Programming/myself/learning/evolution-bootcamp/scala-bootcamp/src/main/resources/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}
