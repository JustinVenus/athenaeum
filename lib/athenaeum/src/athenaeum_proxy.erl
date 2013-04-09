-module(athenaeum_proxy).

-behaviour(gen_server).

% Callback functions which should be exported
-export([init/1,terminate/2,code_change/3]).
-export([handle_call/3,handle_info/2,handle_cast/2]).

% user-defined interface functions
-export([start_link/0]).
-define(TIMEOUT, 1200).

%%FIXME get timeout from application

-record(state, {forward, reverse, parent}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(Parent) ->
    {ok, #state{parent = Parent}}.
handle_call(_What, _From, State) ->
    self() ! ok,
    {reply, ok, State}.

handle_cast({proxy, {Sock1, Sock2}}, State) ->
    inet:setopts(Sock1,[{active,true}, {mode,binary}, {packet, raw}]),
    inet:setopts(Sock2,[{active,true}, {mode,binary}, {packet, raw}]),
    {noreply, State#state{forward = Sock1, reverse = Sock2}, ?TIMEOUT};
handle_cast(What, State) -> 
    io:format("~p~n", [What]),
    {noreply, State, ?TIMEOUT}.

%% all the work happens here
handle_info({tcp, Socket, Data}, #state{forward = F, reverse = R} = State) ->
    case Socket of
        F ->
            gen_tcp:send(R, Data);
        R ->
            gen_tcp:send(F, Data);
        _ -> ok %% ???WTF???
    end,
    {noreply, State, ?TIMEOUT};
handle_info(ok, State) ->
    {noreply, State, ?TIMEOUT};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info({tcp_closed, _} ,State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("Received Unexpected Message (~p)~n", [Msg]),
    {noreply, State, ?TIMEOUT}.

code_change(_Old, State, _Extra) ->
    self() ! ok,
    {ok, State}.
terminate(_Reason, State) -> 
    gen_tcp:close(State#state.forward),
    gen_tcp:close(State#state.reverse),
    ok.
