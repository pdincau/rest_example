-module(auth_service).
-export([is_authorized/1]).

is_authorized(AuthToken) ->
    case AuthToken of
        <<"myusertoken">> ->
            true;
        _ ->
            false
    end.
