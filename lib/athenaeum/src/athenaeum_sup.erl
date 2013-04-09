%%%-------------------------------------------------------------------
%%% @author Justin Venus <justin.venus@gmail.com>
%%% @copyright (C) 2013, Justin Venus
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2013 by Justin Venus <justin.venus@gmail.com>
%%%-------------------------------------------------------------------
-module(athenaeum_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,stop/0]).

%% Supervisor callbacks
-export([init/1]).

-export([
    start_resource/2,
    start_proxy/2,
    stop_resource/2,
    get_resources/1,
    get_pid_by_endpoint/2,
    jsonify_urllist/1,
    jsonify_endpoints/2
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({global, node()}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the supervisor
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    case global:whereis_name(node()) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

start_proxy(Where, Id) when is_atom(Where) ->
    start_proxy(global:whereis_name(Where), Id);
start_proxy(Where, Id) when is_pid(Id) ->
    start_proxy(Where, erlang:pid_to_list(Id));
start_proxy(Where, Id) ->
    ChildSpec = {"proxy("++Id++")", {athenaeum_proxy, start_link, []},
                 temporary, 100, worker, [athenaeum_proxy]},
    supervisor:start_child(Where, ChildSpec).

start_resource(Where, Name) when is_atom(Where) ->
    start_resource(global:whereis_name(Where), Name);
start_resource(Where, Name) when is_pid(Where) ->
    ChildSpec = {Name,
                 {athenaeum_url_sup, start_link, []},
                  permanent, 10500, supervisor, [athenaeum_url_sup]},
    supervisor:start_child(Where, ChildSpec).

stop_resource(Where, Name) when is_atom(Where) ->
    stop_resource(global:whereis_name(Where), Name);
stop_resource(Where, Name) when is_pid(Where) ->
    supervisor:terminate(Where, Name),
    supervisor:delete_child(Where, Name).

get_resources(Where) when is_atom(Where) ->
    get_resources(global:whereis_name(Where));
get_resources(Where) when is_pid(Where) ->
    [{N,P} || {N, P, T,_} <- supervisor:which_children(Where),
     N =/= athenaeum_web_server, 
     T == supervisor,
     is_pid(P)];
get_resources(_Where) -> [].

get_pid_by_endpoint(Name, Where) ->
    case [P || {N,P} <- get_resources(Where), N == Name ] of
       [Head|_] -> Head;
       _ -> {error, nopid}
    end.

jsonify_urllist(Where) when is_atom(Where) ->
    jsonify_urllist(global:whereis_name(Where));
jsonify_urllist(Where) when is_pid(Where) ->
    "[" ++ string:join(
     ["\"" ++ N ++ "\"" || {N, P, T,_} <- supervisor:which_children(Where),
      N =/= athenaeum_web_server, 
      T == supervisor,
      is_pid(P)], ", ") ++ "]".

jsonify_endpoints(Name, Where) ->
    Endpoint = get_pid_by_endpoint(Name, Where),
    case Endpoint of
        {error, nopid} -> {error, nopid};
        Endpoint ->
            Ep = athenaeum_url_sup:get_endpoints(Endpoint),
            "[" ++ string:join(
             ["\"" ++ X ++ "\"" || X <- Ep],
             ", ") ++ "]"
    end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Children = [
	{
		athenaeum_web_server,
		 {athenaeum_web_server, start_link, []},
		  Restart, Shutdown, Type, [athenaeum_web_server]
	}
    ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
