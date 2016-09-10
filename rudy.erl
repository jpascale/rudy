% DS
% Rudy web server
% Server module

-module(rudy).
-export([init/1, handler/1, request/1, reply/1]).

init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->

			gen_tcp:close(Listen),
			ok;
		{error, Error} ->
			error
	end.


handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			:
		{error, Error} ->
		error
	end.

request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		
		{ok, Str} ->
			:
			Response = reply(Request),
			gen_tcp:send(Client, Response);

		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,

	gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
	http:ok(...).