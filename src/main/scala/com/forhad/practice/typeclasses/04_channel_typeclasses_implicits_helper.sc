import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object StringEncoder$ extends ByteEncoder[String] {
    override def encode(a: String) = a.getBytes
  }
}

trait Channel {
  def write[A](a: A)(implicit enc: ByteEncoder[A]): Unit
}

object FileChannel extends Channel {
  override def write[A](a: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(a)
    Using(new FileOutputStream("/Users/forhad/Documents/Programming/myself/learning/evolution-bootcamp/scala-bootcamp/src/main/resources/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}


case class Switch(isOn: Boolean)

object Switch {
  implicit object SwitchEncoder extends ByteEncoder[Switch] {
    override def encode(a: Switch): Array[Byte] =
      Array(if (a.isOn) '1'.toByte else '0'.toByte)
  }
}


//FileChannel.write[String]("omgggh")
FileChannel.write[Switch](Switch(false))
