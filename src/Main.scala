import org.openmuc.jasn1.ber.ReverseByteArrayOutputStream
import tap.DataInterChange

import java.io.{BufferedInputStream, File, FileInputStream}

object Main {

  def main(args: Array[String]): Unit = {
    val path: String = "taps/"
    val arr: Array[File] = new File(path).listFiles.filter(_.getName.startsWith("C"))
    arr.foreach(file => {
      process(path + file.getName)
    })
  }

  private def process(path: String): Unit = {
    val rt: DataInterChange = decodeTap(loadFile(path))
    val mapper = new TapMapper
    mapper.map(rt)
  }

  private def decodeTap(is: BufferedInputStream): DataInterChange = {
    val dic: DataInterChange = new DataInterChange()
    val codeLength: Int = dic.decode(is, null)
    val os: ReverseByteArrayOutputStream = new ReverseByteArrayOutputStream(codeLength, true)
    dic.encode(os)
    dic
  }

  private def loadFile(path: String): BufferedInputStream = {
    val br: BufferedInputStream = new BufferedInputStream(new FileInputStream(path))
    br
  }
}
