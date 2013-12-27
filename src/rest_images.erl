-module(rest_images).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Custom callbacks.
-export([handle_upload_png/2]).

-define(APPLICATION, rest_example).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
            {{<<"image">>, <<"png">>, []}, handle_upload_png}
    ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:binding(image_id, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {ResourceId, Req2} ->
            case file_repository:find(binary_to_integer(ResourceId)) of
                undefined ->
                    {false, Req2, State};
                _ ->
                    {true, Req2, State}
            end
    end.

handle_upload_png(Req, State) ->
    {Data, Req2} = acc_multipart(Req),
    FilePath = new_file_path(file_repository:store("image/png")),
    write_file(full_path(FilePath), Data),
    {true, Req2, State}.

% Private

acc_multipart(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Body, Req2}.

full_path(FileName) ->
    {ok, Dir} = application:get_env(?APPLICATION, destination_dir),
    filename:join([Dir, FileName]).

new_file_path(Id) ->
    PathMd5 = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(integer_to_list(Id))]),
    Dir1 = string:substr(PathMd5, 1, 10),
    Dir2 = string:substr(PathMd5, 11, 10),
    File = string:substr(PathMd5, 21, 12),
    lists:flatten([Dir1, "/", Dir2, "/", File]).

write_file(FullPathFile, Content) ->
    ok = filelib:ensure_dir(FullPathFile),
    ok = file:write_file(FullPathFile, Content).
