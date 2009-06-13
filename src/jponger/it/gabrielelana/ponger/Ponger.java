package it.gabrielelana.ponger;

import java.nio.charset.*;
import org.apache.mina.core.buffer.IoBuffer;
import org.apache.mina.core.service.IoHandler;
import org.apache.mina.core.service.IoHandlerAdapter;
import org.apache.mina.core.session.IdleStatus;
import org.apache.mina.core.session.IoSession;

public class Ponger extends IoHandlerAdapter {

    @Override
    public void sessionOpened(IoSession session) {
        session.getConfig().setIdleTime(IdleStatus.BOTH_IDLE, 60);
    }

    @Override
    public void messageReceived(IoSession session, Object message) {
		IoBuffer requestBuffer = (IoBuffer) message;

		long timestamp = requestBuffer.getLong();

		try {
			String request = requestBuffer.getString(requestBuffer.remaining(), Charset.forName("UTF-8").newDecoder());

			if (request.equals("ping")) {
				// TODO: Its 1:18 in the morning, I don't know why MINA doesn't call the encoder...
				// I have to encode the message directly here
				IoBuffer responseBuffer = IoBuffer.allocate(8 + 4 + 1);
				responseBuffer.put((byte)12);
				responseBuffer.putLong(timestamp);
				responseBuffer.putString("pong", Charset.forName("UTF-8").newEncoder());
				responseBuffer.flip();
				session.write(responseBuffer);
				return;
			}

			if (request.equals("stop")) {
				// TODO: Its 1:18 in the morning, I don't know why MINA doesn't call the encoder...
				// I have to encode the message directly here
				IoBuffer responseBuffer = IoBuffer.allocate(8 + 7 + 1);
				responseBuffer.put((byte)15);
				responseBuffer.putLong(timestamp);
				responseBuffer.putString("stopped", Charset.forName("UTF-8").newEncoder());
				responseBuffer.flip();
				session.write(responseBuffer);
				return;
			}

		} catch(CharacterCodingException exception) { }

    }

    @Override
    public void sessionIdle(IoSession session, IdleStatus status) {
        session.close(true);
    }

    @Override
    public void exceptionCaught(IoSession session, Throwable cause) {
        session.close(true);
    }

}
