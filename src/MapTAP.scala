import java.text.SimpleDateFormat
import java.util.Date

import tap.{DataInterChange, TransferBatch}

class MapTAP {
    def map(dic: DataInterChange): Unit = {
        var imsi: String = ""
        var called: String = ""
        var sender: String = ""
        var calling: String = ""
        var recipient: String = ""

        var typeInfo: Byte = 0
        var eventType: Byte = 0

        var triggerTime: Long = 0
        var callDuration: Long = 0
        var disconnectTime: Long = 0

        val tb: TransferBatch = dic.transferBatch

        if (tb != null) {
            if (tb.batchControlInfo != null) {
            println("TransferBatch -> BatchControlInfo")
                if (tb.batchControlInfo.sender != null)
                    sender = hexToAscii(tb.batchControlInfo.sender.toString)
                    println("Sender: " + sender)
                if (tb.batchControlInfo.recipient != null)
                    recipient = hexToAscii(tb.batchControlInfo.recipient.toString)
                    println("Recipient: " + recipient)

                if (tb.batchControlInfo.fileCreationTimeStamp.localTimeStamp != null) {
                    // Triggertime comes in TAP files as Hex. We convert it to ASCII --> Date --> milliseconds.
                    val triggerTimeString = hexToAscii(tb.batchControlInfo.fileCreationTimeStamp.localTimeStamp.toString)
                    val triggerTimeDate = asciiToDate(triggerTimeString)
                    triggerTime = dateToMillis(triggerTimeDate)
                    println("Trigger time: " + triggerTime + "\n")
                }
            }

            if (tb.callEventDetails != null) {
            println("CallEventDetails\n")
                val cedSize = tb.callEventDetails.seqOf.size()
                for (i <- 0 until cedSize) {
                    val ced = tb.callEventDetails.seqOf.get(i) // To shorten the code a bit.
                    // TransferBatch -> CallEventDetails -> MobileOriginatedCall
                    if (ced.mobileOriginatedCall != null) {
                        typeInfo = 1
                        eventType = 1
                        if (ced.mobileOriginatedCall.basicCallInformation != null) {
                            var connectTime: Long = 0
                            val mocBasicCallInfo = ced.mobileOriginatedCall.basicCallInformation
                            println("TransferBatch -> CallEventDetails -> MobileOriginatedCall")
                            println("Type Info: " + typeInfo)
                            println("Event Type: " + eventType)
                            println("Recipient: " + recipient)

                            if (mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi != null) {
                                imsi = mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi.toString.substring(0, 15)
                                println("IMSI : " + imsi)
                            }
                            if (mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn != null) {
                                calling = mocBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn.toString
                                println("Calling: " + calling)
                            }
                            if (mocBasicCallInfo.destination.calledNumber != null) {
                                called = mocBasicCallInfo.destination.calledNumber.toString
                                println("Called: " + called)
                            }
                            if (mocBasicCallInfo.callEventStartTimeStamp.localTimeStamp != null) {
                                val connectTimeString = hexToAscii(mocBasicCallInfo.callEventStartTimeStamp.localTimeStamp.toString)
                                val connectTimeDate = asciiToDate(connectTimeString)
                                connectTime = dateToMillis(connectTimeDate)
                                println("Connect time: " + fromUnixtime(connectTime))
                            }

                            if (mocBasicCallInfo.totalCallEventDuration.toString != null) {
                                val callDurationString = mocBasicCallInfo.totalCallEventDuration.toString
                                callDuration = callDurationString.toLong
                                println("Duration: " + callDuration)
                            }

                            if (connectTime != 0) {
                                disconnectTime = connectTime + callDuration
                                println("Disconnect Time: " + fromUnixtime(disconnectTime) + "\n")
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
                        }
                    } // TransferBatch -> CallEventDetails -> MobileTerminatedCall
                    else if (ced.mobileTerminatedCall != null) {
                        typeInfo = 2
                        eventType = 1
                        if (ced.mobileTerminatedCall.basicCallInformation != null) {
                            var connectTime: Long = 0
                            val mtcBasicCallInfo = ced.mobileTerminatedCall.basicCallInformation
                            println("TransferBatch -> CallEventDetails -> MobileTerminatedCall")
                            println("Type Info: " + typeInfo)
                            println("Event Type: " + eventType)
                            println("Recipient: " + recipient)

                            if (mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi != null) {
                                imsi = mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.imsi.toString.substring(0, 15)
                                println("IMSI: " + imsi)
                            }
                            if (mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn != null) {
                                called = mtcBasicCallInfo.chargeableSubscriber.simChargeableSubscriber.msisdn.toString
                                println("Called: " + called)
                            }
                            if (mtcBasicCallInfo.callOriginator != null && mtcBasicCallInfo.callOriginator.callingNumber != null) {
                                calling = mtcBasicCallInfo.callOriginator.callingNumber.toString
                                println("Calling: " + calling)
                            }
                            if (mtcBasicCallInfo.callEventStartTimeStamp.localTimeStamp != null) {
                                val connectTimeString = hexToAscii(mtcBasicCallInfo.callEventStartTimeStamp.localTimeStamp.toString)
                                val connectTimeDate = asciiToDate(connectTimeString)
                                connectTime = dateToMillis(connectTimeDate)
                                println("Connect time: " + fromUnixtime(connectTime))
                            }

                            if (mtcBasicCallInfo.totalCallEventDuration.toString != null) {
                                val callDurationString = mtcBasicCallInfo.totalCallEventDuration.toString
                                callDuration = callDurationString.toLong
                                println("Duration: " + callDuration)
                            }

                            if (connectTime != 0) {
                                disconnectTime = connectTime + callDuration
                                println("Disconnect Time: " + fromUnixtime(disconnectTime) + "\n")
                            }

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
                        }
                    }
                }
            }
        }
    }

    final def hexToAscii(hex: String): String = {
        val sb = new StringBuilder
        for (i <- 0 until hex.length by 2) {
            val str = hex.substring(i, i + 2)
            sb.append(Integer.parseInt(str, 16).toChar)
        }
        sb.toString
    }

    final def asciiToDate(date: String): Date = {
        // Use this function if ONLY your date is in the format of "yyyyMMddHHmmss"
        var result: Date = null
        val sdf = new SimpleDateFormat("yyyyMMddHHmmss")
        if(date != null && date.length == 14) {
            result = sdf.parse(date)
        }
        result
    }

    final def dateToMillis(date: Date): Long = {
        date.getTime
    }

    final def fromUnixtime(millis: Long): String = {
        val ts: BigInt = millis
        val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
        val date: String = sdf.format(ts.toLong)
        date
    }
}
