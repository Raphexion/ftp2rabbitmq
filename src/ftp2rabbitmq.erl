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

-record(state, {
	  username,
	  fs,
	  ftpdata,
	  ftpinfo
	 }).

init(InitialState, _) ->
    InitialState.

login(State, Username, _Password) ->
    lager:debug("login"),
    {true, initialize_state(State, Username)}.

current_directory(State) ->
    lager:debug("current directory"),
    {ok, CWD} = erlmemfs:current_directory(fs(State)),
    CWD.

make_directory(State, Directory) ->
    lager:debug("make directory ~p", [Directory]),
    case erlmemfs:make_directory(fs(State), Directory) of
	{ok, _} ->
	    {ok, State};
	_ ->
	    {error, State}
    end.

change_directory(State, Directory) ->
    lager:debug("change directory to ~s", [Directory]),
    case erlmemfs:change_directory(fs(State), Directory) of
	{ok, _Name} ->
	    {ok, State};
	{error, _Why} ->
	    {error, State}
    end.

disconnect(_) ->
    lager:debug("disconnect"),
    ok.

remove_file(State, File) ->
    lager:debug("remove file ~s", [File]),
    case erlmemfs:remove_file(fs(State), File) of
	{ok, File} ->
	    {ok, State};
	{error, Reason} ->
	    lager:warning("remove failed with ~p", [Reason]),
	    {error, State}
    end.

rename_file(State, From, To) ->
    lager:debug("rename file from ~s to ~s", [From, To]),
    case  erlmemfs:rename_file(fs(State), From, To) of
	{ok, _Name} ->
	    {ok, State};
	{error, Reason} ->
	    lager:warning("rename failed with ~p", [Reason]),
	    {error, State}
    end.

remove_directory(State, Directory) ->
    lager:debug("remove directory ~s", [Directory]),
    case erlmemfs:remove_directory(fs(State), Directory) of
	{ok, _Name} ->
	    {ok, State};
	{error, Reason} ->
	    lager:warning("remove directory failed with ~p", [Reason]),
	    {error, Reason}
    end.

list_files(State, "") ->
    list_files(State, ".");
list_files(State, Directory) ->
    lager:debug("list files ~s", [Directory]),
    case erlmemfs:list_files(fs(State), Directory) of
	{ok, Ls} ->
	    transform_into_file_info(Ls);
	{error, Reason} ->
	    lager:warning("ls ~p failed with reason ~p", [Directory, Reason]),
	    []
    end.

% FileRetrievalFun is fun() and returns {ok, Bytes, Count} or done
put_file(State, Filename, _Mode, FileRetrievalFun) ->
    Writer = fun(_) -> FileRetrievalFun() end,
    WriteTimeout = 60000,
    case erlmemfs:put_file(fs(State), Filename, <<>>) of
	{ok, Name} ->
	    {ok, Fp} = erlmemfs:get_file(fs(State), Name),
	    {ok, Fd} = erlmemfs_file:open(Fp),
	    done = erlmemfs_file:write_block(Fp, Fd, Writer, WriteTimeout),
	    erlmemfs_file:close(Fp, Fd),

	    {ok, Path} = erlmemfs:current_directory(fs(State)),
	    start_transfer(Path, Filename, Fp, State),

	    {ok, State};
	{error, Reason} ->
	    lager:warning("put_file failed with ~p", [Reason]),
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

%%

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

initialize_state(State, Username) ->
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    {ok, FtpData} = kiks_producer_sup:add_child("ftpdata"),
    {ok, FtpInfo} = kiks_producer_sup:add_child("ftpinfo"),
    State#connection_state{module_state=#state{username=Username, fs=Fs, ftpdata=FtpData, ftpinfo=FtpInfo}}.

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
		    {ok, Bytes, create_next_fun(State, Fp, Fd)}
	    end
    end.

start_transfer(Path, Filename, Fp, #connection_state{module_state=ModuleState}) ->
    #state{
       username=Username,
       fs=Fs,
       ftpdata=FtpData,
       ftpinfo=FtpInfo
      } = ModuleState,
    file2rabbitmq:start_link(Username, Path, Filename, Fp, FtpData, FtpInfo).
