-module(queue_workers_ets_sup).
-export([start_link/0]).

% The supervisor for all ETS setups

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

% Change into simple_one_for_one, and make
% create_child,

init({}) ->
    % TODO: create child item from sys.config ( data_sources -> ets )
    try_db_init([example_ets_job_table]),
    Children = [ child_spec(list_to_atom("queue_workers_ets_worker_"++integer_to_list(X))) || X <- lists:seq(1, 100) ],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

try_db_init(Tables) ->
    % Sup is heir of ets table.
    % So if proc dies, then table stil exists.
    lists:foreach(fun(Table) ->
        ok = Table:init()
    end, Tables).

child_spec(Id) ->
    {Id,
    {queue_workers_ets_worker, start_link, [Id, example_ets_job_table, example_ets_job_worker]},
    permanent, 5000, worker,
    [queue_workers_ets_worker]}.