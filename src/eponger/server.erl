-module(server).
-export([ start/0, rpc/2, init/1 ]).
-import(lists, [ delete/2 ]).

-define(LISTEN_ON_PORT, 24242).
-define(CONNECTIONS_LIMIT, 15000).


start() ->
	Pid = spawn(?MODULE, init, [ self() ]),
	receive
		{ Pid, started } -> { ok, Pid };
		{ Pid, error, Reason } -> { error, Reason }
	after 10000 ->
		{ error, timeout }
	end.

rpc(Pid, Request) ->
	Pid ! { self(), Request },
	receive
		{ Pid, Response } -> Response
	after 10000 ->
		{ error, timeout }
	end.

init(Parent) ->
	SocketOptions = [ 
		binary, 
		{ packet, 1 },
		{ packet_size, 128 }, 
		{ keepalive, true },
		{ nodelay, false },
		{ active, false }
	],
    process_flag(trap_exit, true),
	case gen_tcp:listen(?LISTEN_ON_PORT, SocketOptions) of
		{ ok, SocketServer } ->
			self() ! { self(), accept },
			Parent ! { self(), started },
			loop(SocketServer, []);
		{ error, Reason } ->
			Parent ! { self(), error, Reason }
	end.

loop(SocketServer, Pongers) ->
	Server = self(),
	receive
		{ _From, accept } when length(Pongers) >= ?CONNECTIONS_LIMIT ->
			io:format("TooBusy: wait a moment please~n"),
			spawn(fun() -> timer:sleep(1000), Server ! { self(), accept } end),
			loop(SocketServer, Pongers);
		{ _From, accept } ->
			ponger:start(SocketServer, Server),
			loop(SocketServer, Pongers);
		{ _From, accepted, Ponger } ->
			catch link(Ponger),
			loop(SocketServer, [ Ponger | Pongers ]);
		{ _From, rejected, Reason } ->
			io:format("RejectedConnection: ~p~n", [ Reason ]),
			loop(SocketServer, Pongers);
		{ _From, closed, Ponger } ->
			io:format("ClosedConnection~n"),
			loop(SocketServer, delete(Ponger, Pongers));
		{ _From, closed, Ponger, Reason } ->
			io:format("ClosedConnection: ~p~n", [ Reason ]),
			loop(SocketServer, delete(Ponger, Pongers));
		{ From, report } ->
			From ! { Server,
				{ 'pongers.connected', length(Pongers) }
			},
			loop(SocketServer, Pongers);
		{ From, stop } ->
			cleanup(SocketServer, Pongers),
			From ! { Server, ok };
		{ 'EXIT', _Ponger, normal } ->
			loop(SocketServer, Pongers);
		{ 'EXIT', Ponger, Reason } ->
			io:format("[~p]Process ~p died because ~p~n", [ ?MODULE, Ponger, Reason ]),
			loop(SocketServer, Pongers);
		NotUnderstood ->
			io:format("[~p]NotUnderstood: ~p~n", [ ?MODULE, NotUnderstood ]),
			loop(SocketServer, Pongers)
	end.

cleanup(SocketServer, Pongers) ->
	lists:map(fun(Ponger) ->
		Ponger ! { stop, server_stopped }
	end, Pongers),
	gen_tcp:close(SocketServer).

