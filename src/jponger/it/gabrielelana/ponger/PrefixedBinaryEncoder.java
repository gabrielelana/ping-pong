package it.gabrielelana.ponger;

import org.apache.mina.core.buffer.IoBuffer;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.filter.codec.ProtocolEncoderAdapter;
import org.apache.mina.filter.codec.ProtocolEncoderOutput;
import org.apache.mina.filter.codec.ProtocolEncoder;

public class PrefixedBinaryEncoder extends ProtocolEncoderAdapter {


    public final static int DEFAULT_PREFIX_LENGTH = 1;
    public final static int DEFAULT_MAX_DATA_LENGTH = 256;

    private int prefixLength = DEFAULT_PREFIX_LENGTH;
    private int maxDataLength = DEFAULT_MAX_DATA_LENGTH;

    public PrefixedBinaryEncoder(int prefixLength) {
        this.prefixLength = prefixLength;
    }

    public void encode(IoSession session, Object message, ProtocolEncoderOutput out) throws Exception {
        IoBuffer messageBuffer = (IoBuffer) message;
        IoBuffer prefixedBuffer = IoBuffer.allocate(messageBuffer.remaining() + prefixLength);
		putPrefix(prefixedBuffer, messageBuffer);
		prefixedBuffer.put(messageBuffer);
        out.write(prefixedBuffer);
    }

	private void putPrefix(IoBuffer prefixedBuffer, IoBuffer messageBuffer) {
		int remaining = messageBuffer.remaining();
        switch (prefixLength) {
        case 1:
			prefixedBuffer.put((byte) remaining);
            break;
        case 2:
			prefixedBuffer.putShort((short)remaining);
            break;
        case 4:
			prefixedBuffer.putInt(remaining);
            break;
        }
	}
}
