% DS
% Rudy web server
% Server module

-module(rudy).
-export([init/1, handler/1, request/1, reply/1, runserver/1, stop/0]).

init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			handler(Listen),
			gen_tcp:close(Listen),
			ok;
		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error]),
			error
	end.


handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			request(Socket);

		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error]),
			error
	end,
	handler(Listen).


request(Socket) ->
	Recv = gen_tcp:recv(Socket, 0),

	case Recv of
		{ok, Str} ->
			Request = http:parse_request(Str),
			Response = reply(Request),
			gen_tcp:send(Socket, Response);

		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,

	gen_tcp:close(Socket).

reply({{get, URI, _}, _, _}) ->
	http:ok("Requested path is " ++ URI).


% RUNSERVER

runserver(Port) ->
	register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
	exit(whereis(rudy), "time to die").






