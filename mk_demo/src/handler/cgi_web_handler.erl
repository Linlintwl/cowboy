-module(cgi_web_handler).

-export([init/2]).


init(Req, State) ->
	Path = cowboy_req:path(Req),
	handle1(Path, Req, State).

handle1(<<"/test">>, Req, State) ->
	ModFunBin = cowboy_req:parse_qs(Req),
	{ok, Bin, Req2} = cowboy_req:read_body(Req),
	Val = jsx:decode(Bin),
	Response = call(ModFunBin, Val),
	Json = jsx:encode(Response),
	Req3 = cowboy_req:reply(200,
		#{<<"content-type">> => <<"text/plain">>},
		Json, Req2),
%%	Req3 = cowboy_req:reply(200, #{
%%		<<"content-type">> => <<"text/plain">>
%%	}, <<"Hello world!">>, Req),
	{ok, Req3, State};
handle1(<<"/cgi">>, Req, State) ->
	Req0 = cowboy_req:stream_reply(200, Req),
	cowboy_req:stream_body("Hello\r\n", nofin, Req0),
	timer:sleep(1000),
	cowboy_req:stream_body("World\r\n", nofin, Req0),
	timer:sleep(1000),
	cowboy_req:stream_body("Chunked!\r\n", fin, Req0),
	{ok, Req0, State};
handle1(<<"/test2">>, Req, State) ->
	BigBody =
	<<"A cowboy is an animal herder who tends cattle on ranches in North America,
	traditionally on horseback, and often performs a multitude of other ranch-
	related tasks. The historic American cowboy of the late 19th century arose
	from the vaquero traditions of northern Mexico and became a figure of special
	significance and legend. A subtype, called a wrangler, specifically tends the
	horses used to work cattle. In addition to ranch work, some cowboys work for
	or participate in rodeos. Cowgirls, first defined as such in the late 19th
	century, had a less-well documented historical role, but in the modern world
	have established the ability to work at virtually identical tasks and obtained
	considerable respect for their achievements. There are also cattle handlers
	in many other parts of the world, particularly South America and Australia,
	who perform work similar to the cowboy in their respective nations.\n">>,
	Req1 = cowboy_req:reply(200, #{}, BigBody, Req),
	{ok, Req1, State};
handle1(Path, Req, State) ->
	Response = read_file(Path),
	Req1 = cowboy_req:reply(200,
		#{<<"content-type">> => <<"text/plain">>},
		Response, Req),
	{ok, Req1, State}.

call([{<<"mod">>,MB},{<<"func">>,FB}], X) ->
	Mod = list_to_atom(binary_to_list(MB)),
	Func = list_to_atom(binary_to_list(FB)),
	apply(Mod, Func, [X]).

read_file(Path) ->
	File = [$.|binary_to_list(Path)],
	case file:read_file(File) of
		{ok, Bin} -> Bin;
		_ -> ["<pre>cannot read:", File, "</pre>"]
	end.