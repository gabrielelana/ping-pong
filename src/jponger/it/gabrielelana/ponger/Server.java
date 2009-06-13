package it.gabrielelana.ponger;

import java.net.InetSocketAddress;

import org.apache.mina.filter.codec.ProtocolCodecFilter;
import org.apache.mina.filter.codec.serialization.ObjectSerializationCodecFactory;
import org.apache.mina.filter.logging.LoggingFilter;
import org.apache.mina.transport.socket.nio.NioSocketAcceptor;

public class Server {
    private static final int SERVER_PORT = 24242;

    public static void main(String[] args) throws Throwable {
        NioSocketAcceptor acceptor = new NioSocketAcceptor();

		acceptor.getFilterChain().addLast("codec", new ProtocolCodecFilter(new PrefixedBinaryCodecFactory(1)));
        acceptor.getFilterChain().addLast("logger", new LoggingFilter());

        acceptor.setHandler(new Ponger());
        acceptor.bind(new InetSocketAddress(SERVER_PORT));

        System.out.println("Listening on port " + SERVER_PORT + "...");
    }
}

