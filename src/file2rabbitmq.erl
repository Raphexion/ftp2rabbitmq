-module(file2rabbitmq).
-behaviour(gen_server).

%% Api

-export([start_link/2]).

%% Behaviour callbacks

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Fp, Mq) ->
    gen_server:start_link(?MODULE, #{fp => Fp, mq => Mq}, []).

init(State=#{fp := Fp, mq := Mq}) ->
    {ok, State, 0}.

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(What, State) ->
    lager:error("unsupported ~p", [What]),
    {noreply, State}.

handle_info(timeout, State=#{fp := Fp, mq := Mq}) ->
    Data = read_data(Fp),
    kiks_producer:send(Mq, Data),
    {noreply, State}.
handle_inf(What, State) ->
    lager:error("unsupported ~p", [What]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:debug("terminate with ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

read_data(Fp) ->
    {ok, Fd} = erlmemfs_file:open(Fp),
    read_data(Fp, Fd, <<>>).

read_data(Fp, Fd, Acc) ->
    case erlmemfs_file:read_block(Fp, Fd, 4096) of
	{ok, eof} ->
	    erlmemfs_file:close(Fp, Fd),
	    Acc;
	{ok, Block} ->
	    Data = erlang:iolist_to_binary([Acc, Block]),
	    read_data(Fp, Fd, Data)
    end.
