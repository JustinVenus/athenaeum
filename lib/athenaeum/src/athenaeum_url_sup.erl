%%%-------------------------------------------------------------------
%%% @author Justin Venus <justin.venus@gmail.com>
%%% @copyright (C) 2013, Justin Venus
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2013 by Justin Venus <justin.venus@gmail.com>
%%%-------------------------------------------------------------------
-module(athenaeum_url_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_endpoints/1, add_child/2]).

%% Supervisor callbacks
-export([init/1]).

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
    supervisor:start_link(?MODULE, []).

get_pids(Sup) when is_pid(Sup) ->
    Result = [P || {_N, P, T, _A} <- supervisor:which_children(Sup),
     T == worker,
     is_pid(P)],
    Result;
get_pids(_Sup) -> [].

add_child(Sup, Host) ->
    ChildSpec = {Host,
                 {athenaeum_host, start_link, []},
                  temporary, 1000, worker, [athenaeum_host]},
    Result = supervisor:start_child(Sup, ChildSpec),
    case Result of
        {ok, Pid} -> gen_server:cast(Pid, {initialize, Host, Sup});
        {error, {already_started, Pid}} -> gen_server:cast(Pid, update);
        Err -> io:format("Error: ~p~n", [Err])
    end,
    Result.

get_endpoints(Sup) ->
    [N || {N, P, T, _A} <- supervisor:which_children(Sup),
     is_pid(P), T == worker].
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
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
