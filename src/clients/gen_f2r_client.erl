-module(gen_f2r_client).
-behaviour(kiks_consumer_protocol).
-behaviour(gen_server).

%% API
-export([start_link/5,
	 process/4]).

%% Behaviour callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% Name, Payload, Topic, Key
-callback client_process(any(), any(), any()) -> any().

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Module, Name, Exchange, Queue, RoutingKey) ->
    gen_server:start_link(?MODULE, [Module, Name, Exchange, Queue, RoutingKey], []).

process(_Tag, Pid, Payload, Key) ->
    gen_server:call(Pid, {process, Payload, Key}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init([Module, Name, Exchange, Queue, RoutingKey]) ->
    {ok, _C} = kiks_consumer_sup:add_child(Exchange, Queue, RoutingKey, ?MODULE, self()),
    {ok, #{name => Name, module => Module}}.

%% @hidden
handle_call({process, Payload, Key}, _From, State) ->
    #{name := Name, module := Mod} = State,
    Res = Mod:client_process(Name, Payload, Key),
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
