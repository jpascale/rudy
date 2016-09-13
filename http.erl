% DS
% Rudy web server
% Http parsing module


-module(http).
-export([parse_request/1, ok/1, get/1, deliver_file/1]).

-define(CR, 13).
-define(LF, 10).
-define(SP, 32).


parse_request(R0) ->
	{Request, R1} = request_line(R0),
	{Headers, R2} = headers(R1),
	{Body, _} = message_body(R2),
	{Request, Headers, Body}.


%%%%%%%%%%%%%%%%%%%%%%
%	Request parser 	 %
%%%%%%%%%%%%%%%%%%%%%%

request_line([$G, $E, $T, ?SP | R0]) ->
	{URI, R1} = request_uri(R0),
	{Ver, R2} = http_version(R1),
	[?CR, ?LF | R3] = R2,
	{{get, URI, Ver}, R3}.

request_uri([?SP | R0])->
	{[], R0};
request_uri([C | R0]) ->
	{Rest, R1} = request_uri(R0),
	{[C | Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
	{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
	{v10, R0}.


%%%%%%%%%%%%%%%%%%%%%%
%	Header parser 	 %
%%%%%%%%%%%%%%%%%%%%%%

headers([?CR, ?LF | R0]) ->
	{[], R0};
headers(R0) ->
	{Header, R1} = header(R0),
	{Rest, R2} = headers(R1),
	{[Header | Rest], R2}.

header([?CR, ?LF| R0]) ->
	{[], R0};
header([C | R0]) ->
	{Rest, R1} = header(R0),
	{[C | Rest], R1}.


%%%%%%%%%%%%%%%%%%%%%%
%	Body parser 	 %
%%%%%%%%%%%%%%%%%%%%%%
%We assume that the rest of the string is the body

message_body(R) ->
	{R, []}.


%%%%%%%%%%%%%%%%%%%%%%
%		Reply	 	 %
%%%%%%%%%%%%%%%%%%%%%%

ok(Body) ->
	"HTTP/1.1 200 Download baby\r\n" ++ "\r\n" ++ Body.

deliver_file(Filename) ->
		
    {ok, IoService} = file:open(Filename, [read]),
    Data = get_all_lines(IoService, []),
    Length = string:len(Data),

	"HTTP/1.0 200 OK\r\n" ++
	"Server: Rudy/1.0\r\n" ++
	"Content-type: application/octet-stream\r\n" ++
	"Content-Disposition: attachment; filename=\"" ++ Filename ++ "\"\r\n" ++
	"Content-Length: " ++ integer_to_list(Length + 141 + string:len(integer_to_list(Length)) + string:len(Filename)) ++ "\r\n\r\n" ++
	Data.

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [Line])
    end.

get(URI) ->
	"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".



