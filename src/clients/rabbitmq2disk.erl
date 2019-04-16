-module(rabbitmq2disk).
-behaviour(kiks_consumer_protocol).
-behaviour(gen_server).

%% API
-export([start_link/4,
	 process/3]).

%% Behaviour callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Name, Exchange, Queue, RoutingKey) ->
    gen_server:start_link(?MODULE, [Name, Exchange, Queue, RoutingKey], []).

process(Pid, Payload, Topic) ->
    gen_server:call(Pid, {process, Payload, Topic}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init([Name, Exchange, Queue, RoutingKey]) ->
    {ok, _C} = kiks_consumer_sup:add_child(Exchange, Queue, RoutingKey, ?MODULE, self()),
    {ok, #{name => Name}}.

%% @hidden
handle_call({process, Payload, Topic}, _From, State) ->
    #{name := Name} = State,
    Res = client_process(Name, Payload, Topic),
    {reply, Res, State};
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

client_process(Name, Payload, Topic) ->
    io:fwrite("[~p] accepting ~p bytes on topic ~p~n", [Name, byte_size(Payload), Topic]),
    ok.
