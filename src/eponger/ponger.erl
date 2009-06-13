-module(ponger).
-export([ start/2, init/2 ]).

start(SocketServer, Server) ->
	spawn(?MODULE, init, [ SocketServer, Server ]).

init(SocketServer, Server) ->
	Ponger = self(),
	case gen_tcp:accept(SocketServer) of
		{ ok, Socket } ->
			inet:setopts(Socket, [ { active, true }, { nodelay, true } ]),
			Server ! { Ponger, accepted, Ponger },
			Server ! { Ponger, accept },
			loop(Server, Socket);
		{ error, Reason } ->
			Server ! { Ponger, rejected, Reason },
			Server ! { Ponger, accept }
    end.

loop(Server, Socket) ->
	Ponger = self(),
	receive
		{ tcp, Socket, Message } ->
			case Message of
				<<Timestamp:64,"ping">> ->
					gen_tcp:send(Socket, <<Timestamp:64,"pong">>);
				<<_Timestamp:64,"stop">> ->
					Ponger ! { stop, game_is_over };
				_Garbage ->
					Ponger ! { stop, garbage_on_socket }
			end,
			loop(Server, Socket);
		{ stop, game_is_over } ->
			Timestamp = timestamp(),
			gen_tcp:send(Socket, <<Timestamp:64,"stopped">>),
			flush_and_bang({ tcp_close, Socket }),
			loop(Server, Socket);
		{ stop, _Reason } ->
			Timestamp = timestamp(),
			gen_tcp:send(Socket, <<Timestamp:64,"error">>),
			flush_and_bang({ tcp_close, Socket }),
			loop(Server, Socket);
		{ tcp_close, Socket } ->
			gen_tcp:close(Socket),
			Ponger ! { tcp_closed, Socket },
			loop(Server, Socket);
		{ tcp_closed, Socket } ->
			Server ! { Ponger, closed, Ponger };
		{ tcp_error, Socket, Reason } -> 
			Server ! { Ponger, closed, Ponger, Reason },
			gen_tcp:shutdown(Socket, read_write);
		NotUnderstood ->
			io:format("[~p]NotUnderstood: ~p~n", [ ?MODULE, NotUnderstood ]),
			loop(Server, Socket)
	after 30000 ->
		Ponger ! { stop, connection_timeout },
		loop(Server, Socket)
	end.
			
flush_and_bang(MessageToSend) ->
	receive
		_AnyMessage -> flush_and_bang(MessageToSend)
	after 0 ->
		self() ! MessageToSend
	end.

timestamp() ->
	{ MegaSeconds, Seconds, MicroSeconds } = now(),
	( MegaSeconds * 1000000000 ) + ( Seconds * 1000 ) + ( MicroSeconds div 1000 ).
