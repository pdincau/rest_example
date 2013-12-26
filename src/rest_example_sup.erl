%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_example_sup).
-behaviour(supervisor).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    Procs = [?CHILD(file_repository, worker)],
    {ok, {{one_for_one, 10, 10}, Procs}}.
