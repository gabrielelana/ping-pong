-module(director).
-export([ start/2, start/4 ]).

-define(TARGET_SERVER_PORT, 24242).
-define(TARGET_SERVER_HOST, "127.0.0.1").

start(NumberOfPingers, NumberOfPingsPerSecond) ->
	spawn(fun() -> warmup(?TARGET_SERVER_HOST, ?TARGET_SERVER_PORT, NumberOfPingers, NumberOfPingsPerSecond) end),
	ok.

start(OnHost, OnPort, NumberOfPingers, NumberOfPingsPerSecond) ->
	spawn(fun() -> warmup(OnHost, OnPort, NumberOfPingers, NumberOfPingsPerSecond) end),
	ok.

warmup(OnHost, OnPort, NumberOfPingers, NumberOfPingsPerSecond) ->
	Director = self(),
	MillisecondsToNextSpawn = round(1000/10),
	Pingers = lists:map(fun(_) ->
		timer:sleep(MillisecondsToNextSpawn),
		spawn(fun() -> pinger:start(OnHost, OnPort, NumberOfPingsPerSecond, Director) end)
	end, lists:seq(1, NumberOfPingers)),
	lists:foreach(fun(Pinger) ->
		Pinger ! run
	end, Pingers),
	loop(Pingers, [], 0),
	ok.

loop(ActivePingers, FinishedPingers, Errors) when ActivePingers =:= [], FinishedPingers =/= [] ->
	io:format("RTD-Median: ~p~n", [ mean([ Median || [ { median, Median }, _ ] <- FinishedPingers ]) ]),  
	io:format("RTD-Deviation: ~p~n", [ mean([ Deviation || [ _, { deviation, Deviation } ] <- FinishedPingers ]) ]),
	io:format("Errors: ~p~n", [ Errors ]),
	ok;
loop(ActivePingers, FinishedPingers, Errors) ->
	receive
		{ collect, Pinger, Data } ->
			Report = [ { median, median(Data) }, { deviation, standard_deviation(Data) } ],
			loop(lists:delete(Pinger, ActivePingers), [ Report | FinishedPingers ], Errors);
		{ error, Pinger } ->
			loop(lists:delete(Pinger, ActivePingers), FinishedPingers, Errors + 1)
	end.

mean(Numbers) ->
    lists:sum(Numbers) / length(Numbers).

median(Numbers) ->
	SortedNumbers = lists:sort(Numbers),
	lists:nth(round((length(Numbers))/2), SortedNumbers).

standard_deviation(Numbers) ->
	Mean = mean(Numbers),
	math:sqrt(mean([ (Value-Mean) * (Value-Mean) || Value <- Numbers ])).
