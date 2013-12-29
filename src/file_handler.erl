-module(file_handler).

-export([read_file/1,
         delete_file/1,
         write_file/2]).

-define(APPLICATION, rest_example).

%% Exported
read_file(Id) ->
    FilePath = generate_full_file_path(Id),
    {ok, Binary} = file:read_file(FilePath),
    Binary.

delete_file(Id) ->
    FilePath = generate_full_file_path(Id),
    ok = file:delete(FilePath).

write_file(Id, Content) ->
    FilePath = generate_full_file_path(Id),
    ok = filelib:ensure_dir(FilePath),
    ok = file:write_file(FilePath, Content).

%% Private
generate_full_file_path(Id) ->
    LocalFilePath = generate_local_file_path(Id),
    {ok, Dir} = application:get_env(?APPLICATION, destination_dir),
    filename:join([Dir, LocalFilePath]).

generate_local_file_path(Id) ->
    PathMd5 = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(integer_to_list(Id))]),
    Dir1 = string:substr(PathMd5, 1, 10),
    Dir2 = string:substr(PathMd5, 11, 10),
    File = string:substr(PathMd5, 21, 12),
    lists:flatten([Dir1, "/", Dir2, "/", File]).

