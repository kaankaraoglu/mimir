import MapperUtils._
import tap.{DataInterChange, TransferBatch}

import java.text.SimpleDateFormat
import java.util.Calendar

class TapMapper {
  def map(dic: DataInterChange): Unit = {
    var sender: String = ""
    var recipient: String = ""
    var triggerTime: Long = 0
    val tb: TransferBatch = dic.transferBatch
    val fileTimeZone: String = "fileTZConst"
    val timezoneMap = scala.collection.mutable.HashMap.empty[String, Long] // timediff [0] is fileCreationTimeStamp

    if (tb != null) {
      if (tb.batchControlInfo != null) {
        if (tb.batchControlInfo.sender != null)
          sender = hexToAscii(tb.batchControlInfo.sender.toString)

        if (tb.batchControlInfo.recipient != null)
          recipient = hexToAscii(tb.batchControlInfo.recipient.toString)

        if (tb.batchControlInfo.fileCreationTimeStamp != null) {
          timezoneMap += (fileTimeZone -> timezoneToLong(hexToAscii(tb.batchControlInfo.fileCreationTimeStamp.utcTimeOffset.toString)))
          if (tb.batchControlInfo.fileCreationTimeStamp.localTimeStamp != null) {
            // Triggertime comes in HEX in TAP files. We convert it to ASCII --> Date --> milliseconds.
            val triggerTimeString = hexToAscii(tb.batchControlInfo.fileCreationTimeStamp.localTimeStamp.toString)
            var triggerTimeDate = asciiToDate(triggerTimeString)
            triggerTime = dateToMillis(triggerTimeDate) + (if (timezoneMap.contains(fileTimeZone)) timezoneMap(fileTimeZone) else 0)
            triggerTimeDate = Calendar.getInstance.getTime
            val df = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
            println("TriggerTime " + df.format(triggerTimeDate))
          }
        }

        if (tb.networkInfo != null && tb.networkInfo.utcTimeOffsetInfo != null) {
          for (i <- 0 until tb.networkInfo.utcTimeOffsetInfo.seqOf.size) {
            timezoneMap += (tb.networkInfo.utcTimeOffsetInfo.seqOf.get(i).utcTimeOffsetCode.toString -> timezoneToLong(hexToAscii(tb.networkInfo.utcTimeOffsetInfo.seqOf.get(i).utcTimeOffset.toString)))
          }
        }

        println("HEADER")
        println("TransferBatch -> BatchControlInfo")
        println("Sender: " + sender)
        println("Recipient: " + recipient + "\n")
        println("Timezone: " + fileTimeZone + "\n")
      }

      if (tb.callEventDetails != null) {
        val cedSize = tb.callEventDetails.seqOf.size()
        for (i <- 0 until cedSize) {
          var imsi: String = ""
          var called: String = ""
          var calling: String = ""
          var typeInfo: Byte = 0
          var eventType: Byte = 0

          var connectTime: Long = 0
          var callDuration: Long = 0
          var disconnectTime: Long = 0

          val ced = tb.callEventDetails.seqOf.get(i) // To shorten the code a bit.

          // TransferBatch -> CallEventDetails -> MobileOriginatedCall
          if (ced.mobileOriginatedCall != null) {
            if (ced.mobileOriginatedCall.basicCallInformation != null) {
              typeInfo = 1
              eventType = 1
              val mocBasicCallInfo = ced.mobileOriginatedCall.basicCallInformation

              if (mocBasicCallInfo.chargeableSubscriber != null) {
                if (mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber != null) {
                  if (mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi != null) {
                    imsi = mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi.toString.substring(0, 15)
                  }

                  if (mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn != null) {
                    calling = mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn.toString
                    //println(calling.replaceAll("F",""))
                  } //else println("Calling is missing")
                }
              }

              if (mocBasicCallInfo.destination != null) {
                if (mocBasicCallInfo.destination.calledNumber != null) {
                  called = mocBasicCallInfo.destination.calledNumber.toString
                } //else println("Called is missing")
              }

              if (mocBasicCallInfo.callEventStartTimeStamp != null) {
                if (mocBasicCallInfo.callEventStartTimeStamp.localTimeStamp != null) {
                  val connectTimeString = hexToAscii(mocBasicCallInfo.callEventStartTimeStamp.localTimeStamp.toString)
                  val connectTimeDate = asciiToDate(connectTimeString)
                  val connectTimeCode =
                    if (mocBasicCallInfo.callEventStartTimeStamp.utcTimeOffsetCode != null) {
                      mocBasicCallInfo.callEventStartTimeStamp.utcTimeOffsetCode.toString
                    } else {
                      ""
                    }

                  val timediff =
                    if (connectTimeCode != null && timezoneMap.contains(connectTimeCode)) {
                      timezoneMap(connectTimeCode)
                    } else if (timezoneMap.contains(fileTimeZone)) {
                      timezoneMap(fileTimeZone)
                    } else {
                      0
                    }
                  connectTime = dateToMillis(connectTimeDate) + timediff
                }
              }

              if (mocBasicCallInfo.totalCallEventDuration.toString != null) {
                // This comes in as seconds.
                // We need it to be in milliseconds so that we can add it to connect time.
                val callDurationString = mocBasicCallInfo.totalCallEventDuration.toString
                val callDurationInSeconds = callDurationString.toLong
                val callDurationInMillis = callDurationInSeconds * 1000
                callDuration = callDurationInMillis
              }

              if (connectTime != 0) {
                if (callDuration != 0) {
                  disconnectTime = connectTime + callDuration
                } else {
                  disconnectTime = connectTime
                }
              }

              if (connectTime > disconnectTime) {
                println("Bad data!")
              }

              if (ced.mobileOriginatedCall.basicServiceUsedList != null) {
                for (j <- 0 until ced.mobileOriginatedCall.basicServiceUsedList.seqOf.size) {
                  val li = ced.mobileOriginatedCall.basicServiceUsedList.seqOf.get(j)
                  if (li.basicService.serviceCode.teleServiceCode != null) {
                    val teleService = hexToAscii(li.basicService.serviceCode.teleServiceCode.toString)
                    if (teleService.equals("22")) {
                      eventType = 2
                    }
                  }
                }
              }
              if (true) {
                println("TransferBatch -> CallEventDetails -> MobileOriginatedCall")
                println("Type Info: " + typeInfo)
                println("Event Type: " + eventType)
                println("Recipient: " + recipient)
                println("IMSI : " + imsi)
                println("Calling: " + calling)
                println("Called: " + called)
                println("Connect time: " + connectTime)
                println("Connect time(fromunixUTC): " + dateFromUnixtime(connectTime))
                println("Call duration: " + callDuration)
                println("Disconnect time: " + disconnectTime)
                println("Disconnect time(fromunixUTC): " + dateFromUnixtime(disconnectTime))
                println("Trigger Time " + triggerTime + "\n")
              }
            }
          }

          // TransferBatch -> CallEventDetails -> MobileTerminatedCall
          else if (ced.mobileTerminatedCall != null) {
            if (ced.mobileTerminatedCall.basicCallInformation != null) {
              typeInfo = 2
              eventType = 1
              val mtcBasicCallInfo = ced.mobileTerminatedCall.basicCallInformation

              if (mtcBasicCallInfo.chargeableSubscriber != null && mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber != null) {
                if (mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi != null) {
                  imsi = mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi.toString.substring(0, 15)
                }

                if (mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn != null) {
                  called = mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn.toString
                }
              }

              if (mtcBasicCallInfo.callOriginator != null) {
                if (mtcBasicCallInfo.callOriginator.callingNumber != null) {
                  calling = mtcBasicCallInfo.callOriginator.callingNumber.toString
                } //else println("Calling is missing")
              }

              if (mtcBasicCallInfo.callEventStartTimeStamp != null && mtcBasicCallInfo.callEventStartTimeStamp.localTimeStamp != null) {
                val connectTimeString = hexToAscii(mtcBasicCallInfo.callEventStartTimeStamp.localTimeStamp.toString)
                val connectTimeDate = asciiToDate(connectTimeString)
                val connectTimeCode =
                  if (mtcBasicCallInfo.callEventStartTimeStamp.utcTimeOffsetCode != null) {
                    mtcBasicCallInfo.callEventStartTimeStamp.utcTimeOffsetCode.toString
                  } else null // bad practice. Fix it!

                val timediff =
                  if (connectTimeCode != null && timezoneMap.contains(connectTimeCode)) {
                    timezoneMap(connectTimeCode)
                  } else if (timezoneMap.contains(fileTimeZone)) {
                    timezoneMap(fileTimeZone)
                  } else {
                    0
                  }
                connectTime = dateToMillis(connectTimeDate) + timediff
              }

              if (mtcBasicCallInfo.totalCallEventDuration != null) {
                val callDurationString = mtcBasicCallInfo.totalCallEventDuration.toString
                val callDurationInSeconds = callDurationString.toLong
                callDuration = callDurationInSeconds * 1000
              }

              if (callDuration != 0) {
                disconnectTime = connectTime + callDuration
              } else {
                disconnectTime = connectTime
              }

              if (connectTime > disconnectTime) {
                println("Bad data!")
              }

              if (connectTime == 0) println("connect time zero!")

              if (ced.mobileTerminatedCall.basicServiceUsedList != null) {
                for (j <- 0 until ced.mobileTerminatedCall.basicServiceUsedList.seqOf.size) {
                  val li = ced.mobileTerminatedCall.basicServiceUsedList.seqOf.get(j)
                  if (li.basicService.serviceCode.teleServiceCode != null) {
                    val teleService = hexToAscii(li.basicService.serviceCode.teleServiceCode.toString)
                    if (teleService.equals("21")) {
                      eventType = 2
                    }
                  }
                }
              }

              //              if (false) {
              //                println("TransferBatch -> CallEventDetails -> MobileTerminatedCall")
              //                println("Type Info: " + typeInfo)
              //                println("Event Type: " + eventType)
              //                println("Recipient: " + recipient)
              //                println("IMSI : " + imsi)
              //                println("Calling: " + calling)
              //                println("Connect time: " + connectTime)
              //                println("Connect time(fromunix_UTC): " + dateFromUnixtime(connectTime))
              //                println("Call duration: " + callDuration)
              //                println("Disconnect time: " + disconnectTime)
              //                println("Disconnect time(fromunixUTC): " + dateFromUnixtime(disconnectTime))
              //                println("Trigger Time " + triggerTime + "\n")
              //              }
            }
          }
        }
      }
    }
  }
}
