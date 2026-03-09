import tap.DataInterChange

import java.io.{BufferedInputStream, File, FileInputStream}
import scala.util.Using

object Main {

  def main(args: Array[String]): Unit = {
    val path = args.headOption.getOrElse("taps/")
    val dir = new File(path)
    if (!dir.isDirectory) {
      println(s"Directory not found: $path")
      return
    }
    val files = Option(dir.listFiles).getOrElse(Array.empty[File]).filter(_.getName.startsWith("C"))
    files.foreach(file => process(file.getAbsolutePath))
  }

  private def process(path: String): Unit = {
    val dic = Using.resource(new BufferedInputStream(new FileInputStream(path))) { is =>
      decodeTap(is)
    }
    val mapper = new TapMapper
    mapper.map(dic)
  }

  private def decodeTap(is: BufferedInputStream): DataInterChange = {
    val dic = new DataInterChange()
    dic.decode(is, null)
    dic
  }
}