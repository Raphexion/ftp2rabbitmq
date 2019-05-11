-module(misc_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([init_per_testcase/2,
	 end_per_testcase/2]).
-export([missing_dir_test/1,
	 cd_test/1]).

all() -> [missing_dir_test,
	  cd_test].

init_per_testcase(_, Config) ->
    application:ensure_all_started(ftp2rabbitmq),
    timer:sleep(500),
    {ok, Ftp} = ftp:start_service([{host, "localhost"}, {port, 2121}]),
    [{ftp,Ftp} | Config].

end_per_testcase(_, Config) ->
    Ftp = ?config(ftp, Config),
    ftp:stop_service(Ftp).

missing_dir_test(Config) ->
    Ftp = ?config(ftp, Config),
    Folder = "abc",
    ok = ftp:user(Ftp, "testtest", "12341234"),
    {error, epath} =:= ftp:cd(Ftp, Folder).

cd_test(Config) ->
    Ftp = ?config(ftp, Config),
    Folder = "abc",
    ok = ftp:user(Ftp, "testtest", "12341234"),
    ok = ftp:mkdir(Ftp, Folder),
    ok =:= ftp:cd(Ftp, Folder).
