-module(prop_dirs).
-include_lib("proper/include/proper.hrl").
-import(prop_generators, [file/0, folder/0, content/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_missing_dir_test() ->
    ?FORALL(Folder, folder(),
	    begin
		{ok, Ftp} = ftp:start_service([{host, "localhost"}, {port, 2121}]),
		ok = ftp:user(Ftp, "testtest", "12341234"),
		{error, epath} =:= ftp:cd(Ftp, Folder)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
