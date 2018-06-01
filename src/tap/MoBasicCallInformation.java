/**
 * This class file was automatically generated by jASN1 v1.9.0 (http://www.openmuc.org)
 */

package tap;

import java.io.IOException;
import java.io.EOFException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.io.UnsupportedEncodingException;
import java.io.Serializable;
import org.openmuc.jasn1.ber.*;
import org.openmuc.jasn1.ber.types.*;
import org.openmuc.jasn1.ber.types.string.*;


public class MoBasicCallInformation implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final BerTag tag = new BerTag(BerTag.APPLICATION_CLASS, BerTag.CONSTRUCTED, 147);

	public byte[] code = null;
	public ChargeableSubscriber chargeableSubscriber = null;
	public RapFileSequenceNumber rapFileSequenceNumber = null;
	public Destination destination = null;
	public DestinationNetwork destinationNetwork = null;
	public CallEventStartTimeStamp callEventStartTimeStamp = null;
	public TotalCallEventDuration totalCallEventDuration = null;
	public SimToolkitIndicator simToolkitIndicator = null;
	public CauseForTerm causeForTerm = null;
	
	public MoBasicCallInformation() {
	}

	public MoBasicCallInformation(byte[] code) {
		this.code = code;
	}

	public MoBasicCallInformation(ChargeableSubscriber chargeableSubscriber, RapFileSequenceNumber rapFileSequenceNumber, Destination destination, DestinationNetwork destinationNetwork, CallEventStartTimeStamp callEventStartTimeStamp, TotalCallEventDuration totalCallEventDuration, SimToolkitIndicator simToolkitIndicator, CauseForTerm causeForTerm) {
		this.chargeableSubscriber = chargeableSubscriber;
		this.rapFileSequenceNumber = rapFileSequenceNumber;
		this.destination = destination;
		this.destinationNetwork = destinationNetwork;
		this.callEventStartTimeStamp = callEventStartTimeStamp;
		this.totalCallEventDuration = totalCallEventDuration;
		this.simToolkitIndicator = simToolkitIndicator;
		this.causeForTerm = causeForTerm;
	}

	public int encode(OutputStream os) throws IOException {
		return encode(os, true);
	}

	public int encode(OutputStream os, boolean withTag) throws IOException {

		if (code != null) {
			for (int i = code.length - 1; i >= 0; i--) {
				os.write(code[i]);
			}
			if (withTag) {
				return tag.encode(os) + code.length;
			}
			return code.length;
		}

		int codeLength = 0;
		if (causeForTerm != null) {
			codeLength += causeForTerm.encode(os, true);
		}
		
		if (simToolkitIndicator != null) {
			codeLength += simToolkitIndicator.encode(os, true);
		}
		
		if (totalCallEventDuration != null) {
			codeLength += totalCallEventDuration.encode(os, true);
		}
		
		if (callEventStartTimeStamp != null) {
			codeLength += callEventStartTimeStamp.encode(os, true);
		}
		
		if (destinationNetwork != null) {
			codeLength += destinationNetwork.encode(os, true);
		}
		
		if (destination != null) {
			codeLength += destination.encode(os, true);
		}
		
		if (rapFileSequenceNumber != null) {
			codeLength += rapFileSequenceNumber.encode(os, true);
		}
		
		if (chargeableSubscriber != null) {
			codeLength += chargeableSubscriber.encode(os, true);
		}
		
		codeLength += BerLength.encodeLength(os, codeLength);

		if (withTag) {
			codeLength += tag.encode(os);
		}

		return codeLength;

	}

	public int decode(InputStream is) throws IOException {
		return decode(is, true);
	}

	public int decode(InputStream is, boolean withTag) throws IOException {
		int codeLength = 0;
		int subCodeLength = 0;
		BerTag berTag = new BerTag();

		if (withTag) {
			codeLength += tag.decodeAndCheck(is);
		}

		BerLength length = new BerLength();
		codeLength += length.decode(is);

		int totalLength = length.val;
		if (totalLength == -1) {
			subCodeLength += berTag.decode(is);

			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(ChargeableSubscriber.tag)) {
				chargeableSubscriber = new ChargeableSubscriber();
				subCodeLength += chargeableSubscriber.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(RapFileSequenceNumber.tag)) {
				rapFileSequenceNumber = new RapFileSequenceNumber();
				subCodeLength += rapFileSequenceNumber.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(Destination.tag)) {
				destination = new Destination();
				subCodeLength += destination.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(DestinationNetwork.tag)) {
				destinationNetwork = new DestinationNetwork();
				subCodeLength += destinationNetwork.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(CallEventStartTimeStamp.tag)) {
				callEventStartTimeStamp = new CallEventStartTimeStamp();
				subCodeLength += callEventStartTimeStamp.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(TotalCallEventDuration.tag)) {
				totalCallEventDuration = new TotalCallEventDuration();
				subCodeLength += totalCallEventDuration.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(SimToolkitIndicator.tag)) {
				simToolkitIndicator = new SimToolkitIndicator();
				subCodeLength += simToolkitIndicator.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			if (berTag.tagNumber == 0 && berTag.tagClass == 0 && berTag.primitive == 0) {
				int nextByte = is.read();
				if (nextByte != 0) {
					if (nextByte == -1) {
						throw new EOFException("Unexpected end of input stream.");
					}
					throw new IOException("Decoded sequence has wrong end of contents octets");
				}
				codeLength += subCodeLength + 1;
				return codeLength;
			}
			if (berTag.equals(CauseForTerm.tag)) {
				causeForTerm = new CauseForTerm();
				subCodeLength += causeForTerm.decode(is, false);
				subCodeLength += berTag.decode(is);
			}
			int nextByte = is.read();
			if (berTag.tagNumber != 0 || berTag.tagClass != 0 || berTag.primitive != 0
			|| nextByte != 0) {
				if (nextByte == -1) {
					throw new EOFException("Unexpected end of input stream.");
				}
				throw new IOException("Decoded sequence has wrong end of contents octets");
			}
			codeLength += subCodeLength + 1;
			return codeLength;
		}

		codeLength += totalLength;

		if (totalLength == 0) {
			return codeLength;
		}
		subCodeLength += berTag.decode(is);
		if (berTag.equals(ChargeableSubscriber.tag)) {
			chargeableSubscriber = new ChargeableSubscriber();
			subCodeLength += chargeableSubscriber.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
			subCodeLength += berTag.decode(is);
		}
		
		if (berTag.equals(RapFileSequenceNumber.tag)) {
			rapFileSequenceNumber = new RapFileSequenceNumber();
			subCodeLength += rapFileSequenceNumber.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
			subCodeLength += berTag.decode(is);
		}
		
		if (berTag.equals(Destination.tag)) {
			destination = new Destination();
			subCodeLength += destination.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
			subCodeLength += berTag.decode(is);
		}
		
		if (berTag.equals(DestinationNetwork.tag)) {
			destinationNetwork = new DestinationNetwork();
			subCodeLength += destinationNetwork.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
			subCodeLength += berTag.decode(is);
		}
		
		if (berTag.equals(CallEventStartTimeStamp.tag)) {
			callEventStartTimeStamp = new CallEventStartTimeStamp();
			subCodeLength += callEventStartTimeStamp.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
			subCodeLength += berTag.decode(is);
		}
		
		if (berTag.equals(TotalCallEventDuration.tag)) {
			totalCallEventDuration = new TotalCallEventDuration();
			subCodeLength += totalCallEventDuration.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
			subCodeLength += berTag.decode(is);
		}
		
		if (berTag.equals(SimToolkitIndicator.tag)) {
			simToolkitIndicator = new SimToolkitIndicator();
			subCodeLength += simToolkitIndicator.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
			subCodeLength += berTag.decode(is);
		}
		
		if (berTag.equals(CauseForTerm.tag)) {
			causeForTerm = new CauseForTerm();
			subCodeLength += causeForTerm.decode(is, false);
			if (subCodeLength == totalLength+2) { subCodeLength-=2; is.reset(); }
			if (subCodeLength == totalLength) {
				return codeLength;
			}
		}
		throw new IOException("Unexpected end of sequence, length tag: " + totalLength + ", actual sequence length: " + subCodeLength);

		
	}

	public void encodeAndSave(int encodingSizeGuess) throws IOException {
		ReverseByteArrayOutputStream os = new ReverseByteArrayOutputStream(encodingSizeGuess);
		encode(os, false);
		code = os.getArray();
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		appendAsString(sb, 0);
		return sb.toString();
	}

	public void appendAsString(StringBuilder sb, int indentLevel) {

		sb.append("{");
		boolean firstSelectedElement = true;
		if (chargeableSubscriber != null) {
			sb.append("\n");
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("chargeableSubscriber: ");
			chargeableSubscriber.appendAsString(sb, indentLevel + 1);
			firstSelectedElement = false;
		}
		
		if (rapFileSequenceNumber != null) {
			if (!firstSelectedElement) {
				sb.append(",\n");
			}
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("rapFileSequenceNumber: ").append(rapFileSequenceNumber);
			firstSelectedElement = false;
		}
		
		if (destination != null) {
			if (!firstSelectedElement) {
				sb.append(",\n");
			}
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("destination: ");
			destination.appendAsString(sb, indentLevel + 1);
			firstSelectedElement = false;
		}
		
		if (destinationNetwork != null) {
			if (!firstSelectedElement) {
				sb.append(",\n");
			}
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("destinationNetwork: ").append(destinationNetwork);
			firstSelectedElement = false;
		}
		
		if (callEventStartTimeStamp != null) {
			if (!firstSelectedElement) {
				sb.append(",\n");
			}
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("callEventStartTimeStamp: ");
			callEventStartTimeStamp.appendAsString(sb, indentLevel + 1);
			firstSelectedElement = false;
		}
		
		if (totalCallEventDuration != null) {
			if (!firstSelectedElement) {
				sb.append(",\n");
			}
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("totalCallEventDuration: ").append(totalCallEventDuration);
			firstSelectedElement = false;
		}
		
		if (simToolkitIndicator != null) {
			if (!firstSelectedElement) {
				sb.append(",\n");
			}
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("simToolkitIndicator: ").append(simToolkitIndicator);
			firstSelectedElement = false;
		}
		
		if (causeForTerm != null) {
			if (!firstSelectedElement) {
				sb.append(",\n");
			}
			for (int i = 0; i < indentLevel + 1; i++) {
				sb.append("\t");
			}
			sb.append("causeForTerm: ").append(causeForTerm);
			firstSelectedElement = false;
		}
		
		sb.append("\n");
		for (int i = 0; i < indentLevel; i++) {
			sb.append("\t");
		}
		sb.append("}");
	}

}
