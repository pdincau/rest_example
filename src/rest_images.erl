-module(rest_images).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

%% Custom callbacks.
-export([handle_upload_png/2,
         handle_upload_jpg/2,
         provide_resource/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) ->
    {AuthToken, Req2} = cowboy_req:qs_val(<<"user_token">>, Req),
    case auth_service:is_authorized(AuthToken) of
        true ->
            {true, Req2, State};
        false ->
            {{false, <<"">>}, Req2, State}
    end.

content_types_accepted(Req, State) ->
    {[
            {{<<"image">>, <<"png">>, []}, handle_upload_png},
            {{<<"image">>, <<"jpg">>, []}, handle_upload_jpg}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
            {{<<"image">>, <<"jpg">>, []}, provide_resource},
            {{<<"image">>, <<"png">>, []}, provide_resource}
    ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:binding(image_id, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {Id, Req2} ->
            case valid_resource(Id) of
                false ->
                    {false, Req2, binary_to_integer(Id)};
                true ->
                    {true, Req2, binary_to_integer(Id)}
            end
    end.

provide_resource(Req, Id) ->
    FilePath = file_handler:file_path(Id),
    Body = file_handler:read_file(FilePath),
    {Body, Req, Id}.

delete_resource(Req, Id) ->
    ok = file_repository:delete(Id),
    FilePath = file_handler:file_path(Id),
    file_handler:delete_file(FilePath),
    {true, Req, Id}.

handle_upload_png(Req, State) ->
    handle_upload(Req, State, "image/png").

handle_upload_jpg(Req, State) ->
    handle_upload(Req, State, "image/jpg").

handle_upload(Req, State, ContentType) ->
    Id = file_repository:store(ContentType),
    FilePath = file_handler:file_path(Id),
    case process_body(Req, FilePath) of
        {ok, Req2} ->
            ResourceUrl = resource_url(Req2, Id),
            {{true, ResourceUrl}, Req2, State};
        {error, _Reason} ->
            file_repository:delete(Id),
            {ok, Req2} = cowboy_req:reply(500, Req),
            {halt, Req2, State}
    end.

% Private

resource_url(_Req, Id) ->
    BinaryId = integer_to_binary(Id),
    <<$/, BinaryId/binary>>.

valid_resource(Id) ->
    case file_repository:find(binary_to_integer(Id)) of
        undefined ->
            false;
        _ ->
            true
    end.

process_body(Req, FilePath) ->
    case cowboy_req:stream_body(Req) of
        {ok, Data, Req2} ->
            file_handler:write_file(FilePath, Data),
            process_body(Req2, FilePath);
        {done, Req2} ->
            {ok, Req2};
        {error, Reason} ->
            file_handler:delete_file(FilePath),
            {error, Reason}
    end.
