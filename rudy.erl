% DS
% Rudy web server
% Server module

-module(rudy).
-export([init/1, handler/1, request/1, serve/1, deliver/1, runserver/1, stop/0, action_decide/1]).

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
			{{get, URI, _}, _, _} = Request = http:parse_request(Str),
			case action_decide(URI) of
				{deliver, Filename} ->
					Response = deliver(Filename);
				{serve, _} ->
					Response = serve(Request)
			end, 
			gen_tcp:send(Socket, Response);
			%timer:sleep(10000);

		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,

	gen_tcp:close(Socket).


deliver(Filename) ->
	http:deliver_file(Filename).

%Generic reply
serve( {{get, URI, _}, _, _}) ->
	http:ok("Requested path is " ++ URI).


action_decide(URI) ->
	case string:rstr(URI, "/download?file=") of
		1 ->
			Filename = string:right(URI, string:len(URI) - 15),
			case file:open(Filename, [read]) of
				 {ok, IoDevice} ->
				 	file:close(IoDevice),
				 	{deliver, Filename};
				 {error, Reason} ->
				 	{serve, URI}
			end;
		true ->
			{serve, URI}
	end.


% RUNSERVER

runserver(Port) ->
	register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
	exit(whereis(rudy), "time to die").






