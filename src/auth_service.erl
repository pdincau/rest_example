-module(auth_service).
-export([is_authorized/1]).

is_authorized(AuthToken) ->
    case AuthToken of
        <<"myusertoken">> ->
            true;
        _ ->
            false
    end.

-ifdef(TEST).

valid_authorization_test() ->
    true = is_authorized(<<"myusertoken">>).

invalid_authorization_test() ->
    false = is_authorized(<<"invalidusertoken">>).

-endif.
