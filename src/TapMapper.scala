import MapperUtils._
import tap.{CallEventDetail, DataInterChange}

class TapMapper {

  private val FileTimezoneKey = "fileTZConst"

  def map(dic: DataInterChange): Seq[CallEvent] = {
    val tb = dic.transferBatch
    if (tb == null) return Seq.empty

    val header = extractHeader(tb)
    val events = extractCallEvents(tb, header)

    printHeader(header)
    events.foreach(printCallEvent)

    events
  }

  private def extractHeader(tb: tap.TransferBatch): BatchHeader = {
    val bci = tb.batchControlInfo
    if (bci == null) return BatchHeader("", "", 0L, Map.empty)

    val sender = Option(bci.sender).map(s => hexToAscii(s.toString)).getOrElse("")
    val recipient = Option(bci.recipient).map(r => hexToAscii(r.toString)).getOrElse("")

    val (triggerTime, fileTimezoneOffset) = extractFileTimestamp(bci)
    val networkOffsets = extractNetworkTimezones(tb)
    val allOffsets = fileTimezoneOffset.map(o => networkOffsets + (FileTimezoneKey -> o)).getOrElse(networkOffsets)

    BatchHeader(sender, recipient, triggerTime, allOffsets)
  }

  private def extractFileTimestamp(bci: tap.BatchControlInfo): (Long, Option[Long]) = {
    val fcts = Option(bci.fileCreationTimeStamp).getOrElse(return (0L, None))
    val offset = Option(fcts.utcTimeOffset).map(o => timezoneToOffsetMillis(hexToAscii(o.toString)))
    val triggerTime = for {
      lts <- Option(fcts.localTimeStamp)
      millis <- asciiToEpochMillis(hexToAscii(lts.toString))
    } yield millis + offset.getOrElse(0L)

    (triggerTime.getOrElse(0L), offset)
  }

  private def extractNetworkTimezones(tb: tap.TransferBatch): Map[String, Long] = {
    val info = Option(tb.networkInfo).flatMap(n => Option(n.utcTimeOffsetInfo))
    info.map { utcInfo =>
      (0 until utcInfo.seqOf.size).map { i =>
        val entry = utcInfo.seqOf.get(i)
        entry.utcTimeOffsetCode.toString -> timezoneToOffsetMillis(hexToAscii(entry.utcTimeOffset.toString))
      }.toMap
    }.getOrElse(Map.empty)
  }

  private def extractCallEvents(tb: tap.TransferBatch, header: BatchHeader): Seq[CallEvent] = {
    if (tb.callEventDetails == null) return Seq.empty

    (0 until tb.callEventDetails.seqOf.size).flatMap { i =>
      val ced = tb.callEventDetails.seqOf.get(i)
      if (ced.mobileOriginatedCall != null)
        extractOriginatedCall(ced, header)
      else if (ced.mobileTerminatedCall != null)
        extractTerminatedCall(ced, header)
      else
        None
    }
  }

  private def extractOriginatedCall(ced: CallEventDetail, header: BatchHeader): Option[CallEvent] = {
    val moc = ced.mobileOriginatedCall
    val info = Option(moc.basicCallInformation).getOrElse(return None)

    val (imsi, calling) = extractSimSubscriber(
      Option(info.chargeableSubscriber).flatMap(cs => Option(cs.simChargeableSubscriber))
    )

    val called = for {
      dest <- Option(info.destination)
      num <- Option(dest.calledNumber)
    } yield num.toString

    val connectTime = resolveConnectTime(
      Option(info.callEventStartTimeStamp),
      header.timezoneOffsets
    )

    val callDuration = Option(info.totalCallEventDuration)
      .map(_.toString.toLong * 1000)
      .getOrElse(0L)

    val eventType = resolveEventType(
      Option(moc.basicServiceUsedList),
      smsCode = "22"
    )

    val disconnectTime = if (connectTime != 0) connectTime + callDuration else 0L

    Some(CallEvent(
      typeInfo = 1,
      eventType = eventType,
      recipient = header.recipient,
      imsi = imsi,
      calling = calling,
      called = called.getOrElse(""),
      connectTimeMillis = connectTime,
      callDurationMillis = callDuration,
      disconnectTimeMillis = disconnectTime,
      triggerTimeMillis = header.triggerTimeMillis
    ))
  }

  private def extractTerminatedCall(ced: CallEventDetail, header: BatchHeader): Option[CallEvent] = {
    val mtc = ced.mobileTerminatedCall
    val info = Option(mtc.basicCallInformation).getOrElse(return None)

    val (imsi, called) = extractSimSubscriber(
      Option(info.chargeableSubscriber).flatMap(cs => Option(cs.simChargeableSubscriber))
    )

    val calling = for {
      orig <- Option(info.callOriginator)
      num <- Option(orig.callingNumber)
    } yield num.toString

    val connectTime = resolveConnectTime(
      Option(info.callEventStartTimeStamp),
      header.timezoneOffsets
    )

    val callDuration = Option(info.totalCallEventDuration)
      .map(_.toString.toLong * 1000)
      .getOrElse(0L)

    val eventType = resolveEventType(
      Option(mtc.basicServiceUsedList),
      smsCode = "21"
    )

    val disconnectTime = if (connectTime != 0) connectTime + callDuration else 0L

    Some(CallEvent(
      typeInfo = 2,
      eventType = eventType,
      recipient = header.recipient,
      imsi = imsi,
      calling = calling.getOrElse(""),
      called = called,
      connectTimeMillis = connectTime,
      callDurationMillis = callDuration,
      disconnectTimeMillis = disconnectTime,
      triggerTimeMillis = header.triggerTimeMillis
    ))
  }

  private def extractSimSubscriber(
    simOpt: Option[tap.SimChargeableSubscriber]
  ): (String, String) = {
    val imsi = simOpt.flatMap(s => Option(s.imsi)).map(_.toString.take(15)).getOrElse("")
    val msisdn = simOpt.flatMap(s => Option(s.msisdn)).map(_.toString).getOrElse("")
    (imsi, msisdn)
  }

  private def resolveConnectTime(
    timestampOpt: Option[tap.DateTime],
    timezoneOffsets: Map[String, Long]
  ): Long = {
    val ts = timestampOpt.getOrElse(return 0L)
    val localTs = Option(ts.localTimeStamp).getOrElse(return 0L)

    val millis = asciiToEpochMillis(hexToAscii(localTs.toString)).getOrElse(return 0L)

    val offsetCode = Option(ts.utcTimeOffsetCode).map(_.toString).getOrElse("")
    val offset = timezoneOffsets.getOrElse(offsetCode,
      timezoneOffsets.getOrElse(FileTimezoneKey, 0L)
    )

    millis + offset
  }

  private def resolveEventType(
    serviceListOpt: Option[tap.BasicServiceUsedList],
    smsCode: String
  ): Byte = {
    serviceListOpt.map { serviceList =>
      val isSms = (0 until serviceList.seqOf.size).exists { j =>
        val code = Option(serviceList.seqOf.get(j).basicService.serviceCode.teleServiceCode)
        code.exists(c => hexToAscii(c.toString) == smsCode)
      }
      if (isSms) 2.toByte else 1.toByte
    }.getOrElse(1.toByte)
  }

  private def printHeader(header: BatchHeader): Unit = {
    println("HEADER")
    println("TransferBatch -> BatchControlInfo")
    println(s"Sender: ${header.sender}")
    println(s"Recipient: ${header.recipient}")
    println(s"Trigger Time: ${if (header.triggerTimeMillis > 0) formatEpochMillis(header.triggerTimeMillis) else "N/A"}")
    println()
  }

  private def printCallEvent(event: CallEvent): Unit = {
    val direction = if (event.typeInfo == 1) "MobileOriginatedCall" else "MobileTerminatedCall"
    println(s"TransferBatch -> CallEventDetails -> $direction")
    println(s"Type Info: ${event.typeInfo}")
    println(s"Event Type: ${event.eventType}")
    println(s"Recipient: ${event.recipient}")
    println(s"IMSI: ${event.imsi}")
    println(s"Calling: ${event.calling}")
    println(s"Called: ${event.called}")
    println(s"Connect time: ${event.connectTimeMillis}")
    println(s"Connect time (UTC): ${formatEpochMillis(event.connectTimeMillis)}")
    println(s"Call duration: ${event.callDurationMillis}")
    println(s"Disconnect time: ${event.disconnectTimeMillis}")
    println(s"Disconnect time (UTC): ${formatEpochMillis(event.disconnectTimeMillis)}")
    println(s"Trigger Time: ${event.triggerTimeMillis}")
    if (!event.isValid) println("WARNING: Invalid event data")
    println()
  }
}