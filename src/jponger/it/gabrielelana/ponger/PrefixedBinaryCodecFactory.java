package it.gabrielelana.ponger;

import org.apache.mina.core.buffer.BufferDataException;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.filter.codec.ProtocolCodecFactory;
import org.apache.mina.filter.codec.ProtocolDecoder;
import org.apache.mina.filter.codec.ProtocolEncoder;

public class PrefixedBinaryCodecFactory implements ProtocolCodecFactory {

    private final PrefixedBinaryEncoder encoder;
    private final PrefixedBinaryDecoder decoder;

    public PrefixedBinaryCodecFactory(int prefixLength) {
        encoder = new PrefixedBinaryEncoder(prefixLength);
        decoder = new PrefixedBinaryDecoder(prefixLength);
    }

    public ProtocolEncoder getEncoder(IoSession session) throws Exception {
        return encoder;
    }

    public ProtocolDecoder getDecoder(IoSession session) throws Exception {
        return decoder;
    }
}
