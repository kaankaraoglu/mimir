import java.io.{BufferedInputStream, FileInputStream}

import org.openmuc.jasn1.ber.ReverseByteArrayOutputStream
import tap.DataInterChange

object Main {

    def main(args: Array[String]): Unit = {
        val path: String = "taps/CDGBRMENORNC07417"
        process(path)
    }

    def process(path: String): Unit = {
        val rt: DataInterChange = decodeTap(loadFile(path))
        val mapper = new MapTAP
        mapper.map(rt)
    }

    def decodeTap(is: BufferedInputStream): DataInterChange = {
        val dic: DataInterChange = new DataInterChange()
        val codeLength: Int = dic.decode(is, null)
        val os: ReverseByteArrayOutputStream = new ReverseByteArrayOutputStream(codeLength, true)
        dic.encode(os)
        dic
    }

    def loadFile(path: String): BufferedInputStream = {
        val br: BufferedInputStream = new BufferedInputStream(new FileInputStream(path))
        br
    }
}
