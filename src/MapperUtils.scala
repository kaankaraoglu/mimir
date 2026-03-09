import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

object MapperUtils {

  private val tapDateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
  private val displayFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  /** Parse a TAP-format date string ("yyyyMMddHHmmss") into epoch millis at UTC. */
  final def asciiToEpochMillis(date: String): Option[Long] = {
    if (date != null && date.length == 14) {
      val ldt = LocalDateTime.parse(date, tapDateFormat)
      Some(ldt.toInstant(ZoneOffset.UTC).toEpochMilli)
    } else {
      None
    }
  }

  /** Format epoch millis as a human-readable UTC datetime string. */
  final def formatEpochMillis(millis: Long): String = {
    val instant = Instant.ofEpochMilli(millis)
    displayFormat.format(instant.atZone(ZoneOffset.UTC))
  }

  /** Decode a hex-encoded string to its ASCII representation. */
  final def hexToAscii(hex: String): String = {
    val sb = new StringBuilder(hex.length / 2)
    for (i <- 0 until hex.length by 2) {
      sb.append(Integer.parseInt(hex.substring(i, i + 2), 16).toChar)
    }
    sb.toString
  }

  /**
   * Convert a timezone string (e.g. "+0200", "-0530") to a UTC offset in milliseconds.
   * The returned value is the adjustment needed to convert local time to UTC:
   *   utcMillis = localMillis + timezoneToOffsetMillis(tz)
   */
  final def timezoneToOffsetMillis(timezoneStr: String): Long = {
    val sign = timezoneStr.charAt(0)
    val hours = timezoneStr.substring(1, 3).toLong
    val minutes = timezoneStr.substring(3, 5).toLong
    val totalMillis = hours * 3600000 + minutes * 60000

    sign match {
      case '+' => -totalMillis
      case '-' => totalMillis
      case _   => 0L
    }
  }
}