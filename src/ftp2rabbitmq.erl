-module(ftp2rabbitmq).
-include_lib("bifrost/include/bifrost.hrl").

-behaviour(gen_bifrost_server).

% Bifrost callbacks
-export([login/3,
         init/2,
         current_directory/1,
         make_directory/2,
         change_directory/2,
         list_files/2,
         remove_directory/2,
         remove_file/2,
         put_file/4,
         get_file/2,
         file_info/2,
         rename_file/3,
         site_command/3,
         site_help/1,
         disconnect/1]).

-record(state, {fs}).

init(InitialState, _) ->
    InitialState.

login(State, _Username, _Password) ->
    lager:warning("login"),
    {true, initialize_state(State)}.

current_directory(State) ->
    lager:warning("current directory"),
    {ok, CWD} = erlmemfs:current_directory(fs(State)),
    CWD.

make_directory(State, Directory) ->
    lager:warning("make directory ~p", [Directory]),
    case erlmemfs:make_directory(fs(State), Directory) of
	{ok, _} ->
	    {ok, State};
	_ ->
	    {error, State}
    end.

change_directory(State, Directory) ->
    lager:warning("change directory ~p", [Directory]),
    case erlmemfs:change_directory(fs(State), Directory) of
	{ok, Name} ->
	    {ok, State};
	{error, _Why} ->
	    {error, State}
    end.

disconnect(_) ->
    lager:warning("disconnect"),
    ok.

remove_file(State, File) ->
    case erlmemfs:remove_file(fs(State), File) of
	{ok, File} ->
	    {ok, State};
	{error, Why} ->
	    {error, State}
    end.

rename_file(State, From, To) ->
    case  erlmemfs:rename_file(fs(State), From, To) of
	{ok, Name} ->
	    {ok, State};
	{error, Why} ->
	    {error, State}
    end.

remove_directory(State, Directory) ->
    case erlmemfs:remove_directory(fs(State), Directory) of
	{ok, Name} ->
	    {ok, State};
	{error, Reason} ->
	    {error, Reason}
    end.

list_files(State, "") ->
    list_files(State, ".");
list_files(State, Directory) ->
    lager:warning("list files >>~s<<", [Directory]),
    case erlmemfs:list_files(fs(State), Directory) of
	{ok, Ls} ->
	    lager:warning("LS: ~p", [Ls]),
	    transform_into_file_info(Ls);
	{error, Reason} ->
	    lager:warning("LS ~p failed with reason ~p", [Directory, Reason]),
	    []
    end.

% FileRetrievalFun is fun() and returns {ok, Bytes, Count} or done
put_file(State, Filename, _Mode, FileRetrievalFun) ->
    Writer = fun(_) -> FileRetrievalFun() end,
    case erlmemfs:put_file(fs(State), Filename, <<>>) of
	{ok, Name} ->
	    {ok, Fp} = erlmemfs:get_file(fs(State), Name),
	    {ok, Fd} = erlmemfs_file:open(Fp),
	    done = erlmemfs_file:write_block(Fp, Fd, Writer),
	    {ok, State};
	{error, Reason} ->
	    {error, State}
    end.

% Returns {ok, fun(ByteCount)}, which is a function that reads ByteCount byes
% and itself returns a continuation until {done, State} is returned.
get_file(State, Path) ->
    case erlmemfs:get_file(fs(State), Path) of
	{ok, Fp} ->
 	    {ok, create_write_fun(State, Fp)};
	E1={error, missing_file} ->
	    E1;
	E2={error, target_is_dir} ->
	    E2
    end.

% Returns a file_info struct about the given path
file_info(_State, _Path) ->
    {error, not_implemented}.

% SITE command support
site_command(_, _, _) ->
    {error, not_found}.

site_help(_) ->
    {error, not_found}.

% Memory Server-specific Functions

read_from_fun(Fun) ->
    read_from_fun([], 0, Fun).
read_from_fun(Buffer, Count, Fun) ->
    case Fun() of
        {ok, Bytes, ReadCount} ->
            read_from_fun(Buffer ++ [Bytes], Count + ReadCount, Fun);
        done ->
            {ok, Buffer, Count}
    end.

reading_fun(State, Bytes) ->
    reading_fun(State, 0, Bytes).
reading_fun(State, _, []) ->
    fun(_) ->
            {done, State}
    end;
reading_fun(State, Pos, Bytes=[Head|Rest]) ->
    TotalSize = size(Head),
    RemainingBytes = TotalSize - Pos,
    if Pos >= TotalSize ->
            reading_fun(State, 0, Rest);
       true ->
            fun(ByteCount) ->
                    Window = binary:part(Head, Pos, min(RemainingBytes, ByteCount)),
                    ReadCount = size(Window),
                    {ok, Window, reading_fun(State, Pos + ReadCount, Bytes)}
            end
    end.

%%

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

initialize_state(State) ->
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    State#connection_state{module_state=#state{fs=Fs}}.

fs(#connection_state{module_state=#state{fs=Fs}}) ->
    Fs.

transform_into_file_info(Listing) ->
    transform_into_file_info(Listing, []).

transform_into_file_info([], Acc) ->
    Acc;
transform_into_file_info([{Type, Name}|Rest], Acc) ->
    Info = new_file_info(Name, Type, 0),
    transform_into_file_info(Rest, [Info|Acc]).

%% Copied from biforst
new_file_info(Name, Type, Size) ->
    #file_info{name=Name,
               mtime=erlang:localtime(),
               type=Type,
               mode=511, % 0777
               gid=0,
               uid=0,
               size=Size}.

create_write_fun(State, Fp) ->
    {ok, Fd} = erlmemfs_file:open(Fp),
    create_next_fun(State, Fp, Fd).

create_next_fun(State, Fp, Fd) ->
    fun(BlockSize) ->
	    case erlmemfs_file:read_block(Fp, Fd, BlockSize) of
		{ok, eof} ->
		    erlmemfs_file:close(Fp, Fd),
		    {done, State};
		{ok, Bytes} ->
		    io:fwrite("BYTES: ~p~n", [Bytes]),
		    {ok, Bytes, create_next_fun(State, Fp, Fd)}
	    end
    end.
