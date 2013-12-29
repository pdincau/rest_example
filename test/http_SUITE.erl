-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([unsupported_media_type/1,
         unauthorized_user/1,
         not_allowed_method/1,
         create_and_recover_resource/1,
         create_and_delete_resource/1]).

-define(BASE_URL, "http://localhost:").
-define(TOKEN, "myusertoken").

%% ct.
all() ->
    [{group, http}].

groups() ->
    Tests = [
              unauthorized_user,
              unsupported_media_type,
              not_allowed_method,
              create_and_recover_resource,
              create_and_delete_resource
            ],
    [{http, [parallel], Tests}].

init_per_suite(Config) ->
    application:start(inets),
    os:cmd("../../_rel/bin/rest_example start"),
    timer:sleep(1000),
    Config.

end_per_suite(_Config) ->
    os:cmd("../../_rel/bin/rest_example stop"),
    application:stop(inets),
    ok.

init_per_group(http, Config) ->
    Config.

end_per_group(http, _Config) ->
    ok.

%% tests
unauthorized_user(_Config) ->
    Url = ?BASE_URL ++ "8080/images?user_token=anotherusertoken",
    {ok, {{_, 401, "Unauthorized"}, _Headers, _Body}} = httpc:request(post, {Url, [], "image/gif", ""}, [], []).

unsupported_media_type(_Config) ->
    Url = ?BASE_URL ++ "8080/images?user_token=" ++ ?TOKEN,
    {ok, {{_, 415, "Unsupported Media Type"}, _Headers, _Body}} = httpc:request(post, {Url, [], "image/gif", ""}, [], []).

not_allowed_method(_Config) ->
    Url = ?BASE_URL ++ "8080/images",
    {ok, {{_, 405, "Method Not Allowed"}, _Headers, _Body}} = httpc:request(put, {Url, [], "image/gif", ""}, [], []).

create_and_recover_resource(_Config) ->
    Id = create_resource(),
    ImageUrl = ?BASE_URL ++ "8080/images" ++ Id ++ "?user_token=" ++ ?TOKEN,
    {ok, {{_, 200, "OK"}, _Headers, Body}} = httpc:request(get, {ImageUrl, []}, [], []),
    Body = "somebytes".

create_and_delete_resource(_Config) ->
    Id = create_resource(),
    ImageUrl = ?BASE_URL ++ "8080/images" ++ Id ++ "?user_token=" ++ ?TOKEN,
    {ok, {{_, 204, "No Content"}, _Headers, _Body}} = httpc:request(delete, {ImageUrl, []}, [], []).

create_resource() ->
    Url = ?BASE_URL ++ "8080/images?user_token=" ++ ?TOKEN,
    {ok, {{_, 201, "Created"}, Headers, _Body}} = httpc:request(post, {Url, [], "image/png", "somebytes"}, [], []),
    proplists:get_value("location", Headers).
