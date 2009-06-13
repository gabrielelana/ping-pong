package it.gabrielelana.ponger;

import org.apache.mina.core.buffer.BufferDataException;
import org.apache.mina.core.buffer.IoBuffer;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.filter.codec.CumulativeProtocolDecoder;
import org.apache.mina.filter.codec.ProtocolDecoder;
import org.apache.mina.filter.codec.ProtocolDecoderOutput;

import java.nio.charset.Charset;

public class PrefixedBinaryDecoder extends CumulativeProtocolDecoder {

    public final static int DEFAULT_PREFIX_LENGTH = 1;
    public final static int DEFAULT_MAX_DATA_LENGTH = 256;

    private int prefixLength = DEFAULT_PREFIX_LENGTH;
    private int maxDataLength = DEFAULT_MAX_DATA_LENGTH;

    public PrefixedBinaryDecoder(int prefixLength) {
        this.prefixLength = prefixLength;
    }

    protected boolean doDecode(IoSession session, IoBuffer in, ProtocolDecoderOutput out) throws Exception {
        if (in.prefixedDataAvailable(prefixLength, maxDataLength)) {
			int prefix = getPrefix(in);
			IoBuffer message = in.getSlice(prefix);
            out.write(message);
            return true;
        }
        return false;
    }

	private int getPrefix(IoBuffer prefixedMessageBuffer) {
        int prefix = 0;

        switch (prefixLength) {
        case 1:
            prefix = prefixedMessageBuffer.get();
            break;
        case 2:
            prefix = prefixedMessageBuffer.getShort();
            break;
        case 4:
            prefix = prefixedMessageBuffer.getInt();
            break;
        }

		return prefix;
	}

}
