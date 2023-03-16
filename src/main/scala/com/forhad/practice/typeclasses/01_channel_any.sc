
import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

/***
  * trait: Channel
  * usage: channel.write(obj)
  *
  * obj: String / Int / Person(age: Int, name: String)
  *
  *
  * Pros: Simple interface
  * Cons: Unhandled type -> throw exception, two responsibility (transform to bytes, write to channel)
  */
trait Channel {
  def write(obj: Any): Unit
}

object FileChannel extends Channel {
  override def write(obj: Any): Unit = {
    val bytes: Array[Byte] = obj match {
      case n: Int =>
        val bb = ByteBuffer.allocate(4)
        bb.putInt(n)
        bb.array()
      case s: String => s.getBytes
      case invalid => throw new Exception("unhandled ")
    }

    Using(new FileOutputStream("/Users/forhad/Documents/Programming/myself/learning/evolution-bootcamp/scala-bootcamp/src/main/resources/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

FileChannel.write("hello2")
