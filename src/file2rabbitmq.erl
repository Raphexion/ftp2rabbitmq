-module(file2rabbitmq).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).


%% Api

-export([start_link/6,
	 topic/3]).

%% Behaviour callbacks

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Username, Path, Filename, Fp, FtpData, FtpInfo) ->
    Topic = topic(Username, Path, Filename),
    gen_server:start_link(?MODULE, #{fp => Fp, ftpdata => FtpData, ftpinfo => FtpInfo, topic => Topic}, []).

init(State=#{fp := _Fp, ftpdata := _FtpData, ftpinfo := _FtpInfo, topic := _Topic}) ->
    {ok, State, 0}.

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast(What, State) ->
    lager:error("unsupported ~p", [What]),
    {noreply, State}.

handle_info(timeout, State=#{fp := Fp, ftpdata := FtpData, ftpinfo := FtpInfo, topic := Topic}) ->
    Data = read_data(Fp),
    kiks_producer:send(FtpData, Data, Topic),
    {noreply, State};

handle_info(What, State) ->
    lager:error("unsupported ~p", [What]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:debug("terminate with ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------
%% Private
%% ---------------------------------------------------------------------

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

safe_filename(Filename) ->
    [case X of
	 $. ->
	     $_;
	 C ->
	     C
     end || X <- Filename].

safe_path(Path) ->
    Path.

topic(Username, Path, Filename) ->
    SafePath = safe_path(Path),
    SafeFilename = safe_filename(Filename),
    Parts = [Username, SafePath, SafeFilename],
    Topic = string:join(Parts, "."),
    Topic.

%% ---------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------

safe_filename_test_() ->
    ?_assert("abc_def_txt" =:= safe_filename("abc.def.txt")).

topic_test_() ->
    ?_assert("guest.a.b.c.abc_txt" =:= topic("guest", "/a/b/c", "abc.txt")).
