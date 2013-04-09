%%%-------------------------------------------------------------------
%%% @author Justin Venus <justin.venus@gmail.com>
%%% @copyright (C) 2013, Justin Venus
%%% @doc
%%%  generic service endpoint webserver
%%% @end
%%% Created : 16 Mar 2013 by Justin Venus <justin.venus@gmail.com>
%%%-------------------------------------------------------------------
-module(athenaeum_web_server).

-behaviour(gen_web_server).

%% API
-export([start_link/0]).

%% callbacks
-export([
	 init/1,
	 head/3,
	 get/3,
	 delete/3,
	 options/4,
	 post/4,
	 put/4,
	 trace/4,
	 other_methods/4,
         terminate/1
	]).

-record(state, {document_root, socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_link() ->
%%FIXME pull port from configuration
    gen_web_server:start_link(?MODULE, 8080, "/tmp/repo/").

%%%===================================================================
%%% gen_web_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @spec (UserArgs) -> void()
%% @end
%%--------------------------------------------------------------------
init(DocumentRoot) ->
    {ok, #state{document_root = DocumentRoot}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers) -> Response
%% @end
%%--------------------------------------------------------------------
get({http_request,'GET',{abs_path,<<"/">>},Ver}, Headers, State) ->
    get({http_request,'GET',{abs_path,<<"/registry/">>},Ver}, Headers, State);
get({http_request,'GET',{abs_path,<<"/registry">>},Ver}, Headers, State) ->
    get({http_request,'GET',{abs_path,<<"/registry/">>},Ver}, Headers, State);
get({http_request,'GET',{abs_path,<<"/registry/">>},_Ver}, Headers, State) ->
    Body = "{\"/registry\": " ++ athenaeum_sup:jsonify_urllist(node()) ++ "}",
    {stop, gen_web_server:http_reply(200, 
        lists:append(Headers,[{'Content-Type',<<"application/json">>}]), Body), State};
get({http_request,'GET',{abs_path,<<"/registry/",Rest/binary>>},_Ver}, Headers, State) ->
    get({http_request,'GET',{abs_path,Rest},_Ver}, Headers, State);
get({http_request,'GET',{abs_path,Rest},_Ver}, Headers, State) ->
    Regi = "/" ++ strip_trailing_slash(Rest),
    case athenaeum_sup:jsonify_endpoints(Regi, node()) of
       {error, nopid} ->
           Body = "Not registered: " ++ Regi,
           {stop, gen_web_server:http_reply(404, 
               lists:append(Headers,[{'Content-Type',<<"text/plain">>}]), Body), State};
       "[]" ->
           Body = "No membership: " ++ Regi,
           {stop, gen_web_server:http_reply(503, 
               lists:append(Headers,[{'Content-Type',<<"text/plain">>}]), Body), State};
       Data -> 
           Body = "{\"" ++ Regi ++ "\": " ++ Data ++ "}",
           {stop, gen_web_server:http_reply(200, 
               lists:append(Headers,[{'Content-Type',<<"application/json">>}]), Body), State}
    end;
get(_RequestLine, Headers, State) ->
    Body = "<h2>Welcome to the gen_web_server</h2>"
           "<p>Docs can be found at erlware.org or by"
           " generating edocs on the app</p>",
    {stop, gen_web_server:http_reply(200, Headers, Body), State}.

head(_RequestLine, _Headers, _State) -> {stop, gen_web_server:http_reply(200), []}.
delete(_RequestLine, _Headers, _State) -> {gen_web_server:http_reply(200), []}.

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body, State) -> Response
%% @end
%%--------------------------------------------------------------------
put({http_request,'PUT',{abs_path,<<"/register/",Rest/binary>>},_Ver}, 
        _Headers, Body, State) -> 
    Host = strip_trailing_slash(Rest),
    %%remove whitespace from user input
    NoWhite = [X||X <- binary_to_list(Body), X =/= $ ],
    update_endpoint(string:tokens(NoWhite,","), Host),
    {stop, gen_web_server:http_reply(204), State};
put(_RequestLine, _Headers, _Body, _State) -> {stop, gen_web_server:http_reply(501), []}.
trace(_RequestLine, _Headers, _Body, _State) -> {stop, gen_web_server:http_reply(200), []}.
post(_RequestLine, _Headers, _Body, _State) -> {stop, gen_web_server:http_reply(200), []}.
options(_RequestLine, _Headers, _Body, _State) -> {stop, gen_web_server:http_reply(200), []}.
other_methods(
        {http_request,<<"CONNECT">>,
         {scheme,<<BinHost/binary>>,<<BinPort/binary>>},_Ver}, Headers, _Body, State) ->
    io:format("Here ~p ~p~n", [BinHost, BinPort]),
    case make_connection({binary, {BinHost, BinPort}}) of
        {ok, Socket} ->
            NewState = State#state{socket = Socket},
            io:format("~p~n", [Socket]),
            {proxy, <<"HTTP/1.1 200 Connection established\r\n\r\n">>, NewState};
        Error ->
            io:format("~p~n", [Error]),
            Body = "Generic Error",
            {stop, gen_web_server:http_reply(404, Headers, Body), State}
    end;
other_methods({http_request,<<"APPOINT">>,{abs_path,Rest},_Ver}, Headers, _Body, State) ->
    Endpoint = "/" ++ strip_trailing_slash(Rest),
    case athenaeum_sup:get_pid_by_endpoint(Endpoint, node()) of
        {error, nopid} ->
            Body = "No such endpoint " ++ Endpoint,
            {stop, gen_web_server:http_reply(404, Headers, Body), State};
        Pid ->
            L = athenaeum_url_sup:get_endpoints(Pid),
            case make_connection([X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])]) of
                {ok, Socket} ->
                    NewState = State#state{socket = Socket},
                    {proxy, <<"HTTP/1.1 200 Connection established\r\n\r\n">>, NewState};
%%FIXME detail errors
                {error, _} ->
                    Body = "Generic Error",
                    {stop, gen_web_server:http_reply(404, Headers, Body), State}
            end
    end;
other_methods({proxy_request,ClientSocket}, _Headers, _Body, State) ->
    {stop, proxy_connections(State#state.socket, ClientSocket), State};
other_methods(RequestLine, _Headers, _Body, _State) ->
    io:format("Catch all~p~n", [RequestLine]),
    {stop, gen_web_server:http_reply(200), []}.


%%--------------------------------------------------------------------
%% @doc
%% @spec (State) -> ok
%% @end
%%--------------------------------------------------------------------
terminate(_State) -> ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
proxy_connections(Socket1, Socket2) ->
    {ok, Pid} = athenaeum_sup:start_proxy(node(), self()),
    ok = gen_tcp:controlling_process(Socket1, Pid),
    ok = gen_tcp:controlling_process(Socket2, Pid),
    gen_server:cast(Pid, {proxy, {Socket1, Socket2}}),
    ok.

make_connection({binary, {Host, Port}}) ->
    io:format("Here2~n"),
    {P,_} = string:to_integer(erlang:binary_to_list(Port)),
    io:format("Here2 ~p~n", [P]),
    H = erlang:binary_to_list(Host),
    io:format("Here2 ~p~n", [H]),
    Result = gen_tcp:connect(
        H, P, [{active, false}, {mode, binary}], 120),
    io:format("Connection ~p~n", [Result]), Result;
make_connection([]) -> {error, "no endpoint"};
make_connection([H|T]) ->
    case make_connection({connect, H}) of
        {ok, Socket} -> {ok, Socket};
        _ -> make_connection(T)
    end;
make_connection({connect, H}) ->
    case string:tokens(H, ":") of
        [Host, StrPort] ->
            {Port,_} = string:to_integer(StrPort),
            gen_tcp:connect(Host, Port, [
                {active, false}, {mode, binary}], 120);
        _ -> {error, "illegal registration " ++ H}
    end.

update_endpoint([H|T], Host) ->
    update_endpoint(H, [node()|nodes()], Host),
    update_endpoint(T, Host);
update_endpoint([], _Host) -> ok.
update_endpoint(E, [H|T], Host) ->
    case athenaeum_sup:start_resource(H, E) of
        {ok, Pid} -> ok;
        {error, {already_started,Pid}} -> ok;
        {error, {{already_started,Pid}, _}} -> ok;
        _ -> Pid = 0
    end,
    case is_pid(Pid) of
        true -> athenaeum_url_sup:add_child(Pid, Host);
        _ -> ok
    end,
    update_endpoint(E, T, Host);
update_endpoint(_E, [], _Host) -> ok.

strip_trailing_slash(Input) when is_binary(Input) ->
    strip_trailing_slash(binary_to_list(Input));
strip_trailing_slash(Input) ->
    string:join(string:tokens(Input, "/"), "/").
