-module(athenaeum_host).

-behaviour(gen_server).

% Callback functions which should be exported
-export([init/1,terminate/2,code_change/3]).
-export([handle_call/3,handle_info/2,handle_cast/2]).

% user-defined interface functions
-export([start_link/0]).

%%FIXME get timeout from application

-record(state, {super, hoststring, host, port}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, #state{}}.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast({initialize, HostInfo, Super}, _State) ->
    [Host,P] = string:tokens(HostInfo, ":"),
    {Port, _} = string:to_integer(P),
    {noreply, #state{super=Super, hoststring=HostInfo, host=Host, port=Port}};

handle_cast(timeout, State) -> {stop, normal, State};
handle_cast(_What, State) -> {noreply, State}.

handle_info(Msg, State) ->
    io:format("Received Unexpected Message (~p)~n", [Msg]),
    {noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
