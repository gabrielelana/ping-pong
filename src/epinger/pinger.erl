-module(pinger).
-export([ start/4 ]).

-define(NUMBER_OF_PINGS, 100).

start(OnHost, OnPort, NumberOfPingsPerSecond, Director) ->
	SocketOptions = [ binary, { packet, 1 }, { packet_size, 128 }, { active, true }, { nodelay, true } ],
	{ ok, Socket } = gen_tcp:connect(OnHost, OnPort, SocketOptions),

	MillisecondsPerSecond = 1000,
	MillisecondsToNextPing = round(MillisecondsPerSecond/NumberOfPingsPerSecond),
	{ ok, Timer } = timer:send_interval(MillisecondsToNextPing, ping),

	loop(Socket, Director, Timer).

loop(Socket, Director, Timer) -> 
	loop(Socket, Director, Timer, 0, warmup, []).

loop(Socket, Director, Timer, Counter, Modality, Data) ->
	receive
		ping when Modality =:= warmup ->
			Timestamp = timestamp(),
			gen_tcp:send(Socket, <<Timestamp:64,"ping">>),
			loop(Socket, Director, Timer, Counter, warmup, Data);

		ping when Modality =:= run, Counter >= ?NUMBER_OF_PINGS ->
			Timestamp = timestamp(),
			% io:format("-> STOP~n"),
			gen_tcp:send(Socket, <<Timestamp:64,"stop">>),
			loop(Socket, Director, Timer, Counter, Modality, Data);

		ping when Modality =:= run ->
			Timestamp = timestamp(),
			% io:format("-> PING/~p~n", [ Counter ]),
			gen_tcp:send(Socket, <<Timestamp:64,"ping">>),
			loop(Socket, Director, Timer, Counter + 1, run, Data);

		run ->
			loop(Socket, Director, Timer, Counter, run, Data);

		finished ->
			Director ! { collect, self(), Data };

		error ->
			Director ! { error, self() };

		{ stop, Reason } ->
			timer:cancel(Timer),
			gen_tcp:close(Socket),
			flush_and_bang(Reason),
			loop(Socket, Director, Timer, Counter, Modality, Data);

		{ tcp, Socket, Message } ->
			case Message of
				<<Timestamp:64,"pong">> when Modality =:= run ->
					UpdatedData = [ timestamp() - Timestamp | Data ],
					% io:format("<- PONG~n"),
					ok;
				<<_Timestamp:64,"pong">> when Modality =:= warmup ->
					UpdatedData = Data,
					doNothing;
				<<_Timestamp:64,"stopped">> ->
					UpdatedData = Data,
					flush_and_bang({ stop, finished }),
					doNothing;
				<<_Timestamp:64,"error">> ->
					UpdatedData = Data,
					flush_and_bang({ stop, error }),
					doNothing;
				_Garbage ->
					UpdatedData = Data,
					flush_and_bang({ stop, error }),
					ok
			end,
			loop(Socket, Director, Timer, Counter, Modality, UpdatedData);

		{ tcp_closed, Socket } ->
			self() ! error,
			loop(Socket, Director, Timer, Counter, Modality, Data);

		{ tcp_error, Socket, _Reason } -> 
			self() ! error,
			loop(Socket, Director, Timer, Counter, Modality, Data);

		NotUnderstood ->
			io:format("[~p]NotUnderstood: ~p~n", [ ?MODULE, NotUnderstood ]),
			loop(Socket, Director, Timer, Counter, Modality, Data)
	end.


timestamp() ->
	{ MegaSeconds, Seconds, MicroSeconds } = now(),
	( MegaSeconds * 1000000000 ) + ( Seconds * 1000 ) + ( MicroSeconds div 1000 ).
			
flush_and_bang(MessageToSend) ->
	receive
		_AnyMessage -> flush_and_bang(MessageToSend)
	after 0 ->
		self() ! MessageToSend
	end.
