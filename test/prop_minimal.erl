-module(prop_minimal).
-include_lib("proper/include/proper.hrl").
-import(prop_generators, [file/0, folder/0, content/0, unique_folder_and_file/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_ftpinfo_test() ->
    ?FORALL({{Folder, Filename}, Content}, {unique_folder_and_file(), content()},
	    begin
		Exchange = "ftpinfo",
		RandomQueue = "",
		AllTopics = "#",
		Retries = 100,
		Timeout = 20,

		application:ensure_all_started(ftp2rabbitmq),
		{ok, Q} = kiks_queue:start_link(Exchange, RandomQueue, AllTopics),
		empty_queue(Q),

		send(Folder, Filename, Content),
		false =:= kiks_queue:empty(Q)
	    end).

prop_ftpdata_test() ->
    ?FORALL({{Folder, Filename}, Content}, {unique_folder_and_file(), content()},
	    begin
		Exchange = "ftpdata",
		RandomQueue = "",
		AllTopics = "#",
		Retries = 100,
		Timeout = 20,

		application:ensure_all_started(ftp2rabbitmq),
		{ok, Q} = kiks_queue:start_link(Exchange, RandomQueue, AllTopics),
		empty_queue(Q),

		send(Folder, Filename, Content),
		false =:= kiks_queue:empty(Q) andalso
		    Content =:= kiks_queue:pop(Q)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

empty_queue(Q) ->
    empty_queue(Q, kiks_queue:empty(Q)).

empty_queue(Q, true) ->
    ok;
empty_queue(Q, false) ->
    kiks_queue:pop(Q),
    empty_queue(Q).

send(Folder, Filename, Content) ->
    {ok, Ftp} = ftp:start_service([{host, "localhost"}, {port, 2121}]),
    ok = ftp:user(Ftp, "testtest", "12341234"),
    ok = ftp:mkdir(Ftp, Folder),
    ok = ftp:cd(Ftp, Folder),
    ok = ftp:send_bin(Ftp, Content, Filename),
    ok = ftp:close(Ftp).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
