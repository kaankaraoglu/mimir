/** Strongly-typed representation of a decoded TAP call event. */
case class CallEvent(
  typeInfo: Byte,
  eventType: Byte,
  recipient: String,
  imsi: String,
  calling: String,
  called: String,
  connectTimeMillis: Long,
  callDurationMillis: Long,
  disconnectTimeMillis: Long,
  triggerTimeMillis: Long
) {
  def isValid: Boolean = connectTimeMillis > 0 && connectTimeMillis <= disconnectTimeMillis
}

/** Strongly-typed representation of decoded TAP batch header info. */
case class BatchHeader(
  sender: String,
  recipient: String,
  triggerTimeMillis: Long,
  timezoneOffsets: Map[String, Long]
)