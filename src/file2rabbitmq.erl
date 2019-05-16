-module(file2rabbitmq).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).


%% Api

-export([start_link/6]).

%% Behaviour callbacks

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Username, Path, Filename, Fp, FtpData, FtpInfo) ->
    gen_server:start_link(?MODULE, {Username, Path, Filename, Fp, FtpData, FtpInfo}, []).

init(State={_Username, _Path, _Filename, _Fp, _FtpData, _FtpInfo}) ->
    {ok, State, 0}.

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(What, State) ->
    lager:error("unsupported ~p", [What]),
    {noreply, State}.

handle_info(timeout, State) ->
    send_data(State),
    send_info(State),
    {noreply, State};

handle_info(What, State) ->
    lager:error("unsupported ~p", [What]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:warning("terminate with ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------
%% Private
%% ---------------------------------------------------------------------

send_data({Username, _Path, _Filename, Fp, FtpData, _FtpInfo}) ->
    Topic = Username,
    Data = read_data(Fp),
    kiks_producer:send(FtpData, Data, Topic).

send_info({Username, Path, Filename, Fp, _FtpData, FtpInfo}) ->
    Topic = Username,
    {ok, Hash} = erlmemfs_file:hash(Fp),
    Info = jiffy:encode(#{username => Username,
			  path => Path,
			  filename => Filename,
			  hash => Hash}),
    kiks_producer:send(FtpInfo, Info, Topic).

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
