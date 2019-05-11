-module(prop_generators).
-include_lib("proper/include/proper.hrl").

-export([file/0,
	 content/0,
	 folder/0,
	 folders/0,
	 unique_folder_and_file/0]).

ftp_safe_char() ->
    range(48, 127).

ftp_safe_string() ->
    ?SUCHTHAT(String, list(ftp_safe_char()), String /= " ").

file() ->
    non_empty(ftp_safe_string()).

content() ->
    non_empty(binary()).

folder() ->
    ?SUCHTHAT(Folder, non_empty(ftp_safe_string()), not invalid(Folder)).

folders() ->
    non_empty(list(folder())).

invalid(Folder) ->
    lists:member($/, Folder) or (Folder =:= ".") or (Folder =:= "..").

unique_folder_and_file() ->
    ?SUCHTHAT({Folder, File}, {folder(), file()}, Folder /= File).
