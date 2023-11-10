import java.text.SimpleDateFormat
import java.util.Date

object MapperUtils {
  // Use this function if ONLY your date is in the format of "yyyyMMddHHmmss"
  final def asciiToDate(date: String): Date = {
    var result: Date = null
    val sdf = new SimpleDateFormat("yyyyMMddHHmmss")
    if (date != null && date.length == 14) {
      result = sdf.parse(date)
    }
    result
  }

  final def dateToMillis(date: Date): Long = {
    date.getTime
  }

  final def dateFromUnixtime(millis: Long): String = {
    val ts: BigInt = millis
    val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val date: String = sdf.format(ts.toLong)
    date
  }

  final def hexToAscii(hex: String): String = {
    val sb = new StringBuilder
    for (i <- 0 until hex.length by 2) {
      val str = hex.substring(i, i + 2)
      sb.append(Integer.parseInt(str, 16).toChar)
    }
    sb.toString
  }

  /**
   * Input: timezone in the format: "+0000"
   * Returns: diff from UTC in milliseconds
   * */
  final def timezoneToLong(timezoneStr: String): Long = {
    var calculatedTimeZone: Long = 0
    val timeZoneSign: String = timezoneStr.substring(0, 1)
    val timezoneHour: Long = timezoneStr.substring(1, 3).toLong
    val timezoneMinute: Long = timezoneStr.substring(3, 5).toLong

    if (timeZoneSign == "+") {
      calculatedTimeZone -= (timezoneHour * 3600000 + timezoneMinute * 60000)
    } else if (timeZoneSign == "-") {
      calculatedTimeZone += (timezoneHour * 3600000 + timezoneMinute * 60000)
    }
    calculatedTimeZone
  }

}
