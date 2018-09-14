-module(queue_workers_ets_sup).
-export([start_link/2]).

% The supervisor for all ETS setups

-behaviour(supervisor).
-export([init/1]).

-export([worker_name/2]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(Name, Opts) ->
    supervisor:start_link({local, Name}, ?MODULE, {Name, Opts}).

% Change into simple_one_for_one, and make
% create_child,

init({Name, Opts}) ->
    Name = ets:new(Name, [named_table, public, ordered_set]),
    {table_name, TblName} = lists:keyfind(table_name, 1, Opts),
    {worker_count, WorkerCount} = lists:keyfind(worker_count, 1, Opts),
    {worker_module, WorkerMod} = lists:keyfind(worker_module, 1, Opts),
    ok = try_db_init(TblName),
    Children = [ 
        begin 
            Id = list_to_atom(atom_to_list(TblName)++"_worker_"++integer_to_list(X)),
            true = ets:insert(Name, {X, Id}),
            child_spec(Id, WorkerMod)
        end
        || X <- lists:seq(1, WorkerCount) 
    ],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, 
            [ ?CHILD(queue_workers_ets_work_coordinator, worker, [Name, TblName, WorkerCount]) | 
              Children ]
         }
    }.

try_db_init(TblName) ->
    % Sup is heir of ets table.
    % So if proc dies, then table stil exists.
    ok = queue_workers_ets_tbl:init(TblName).

worker_name(Name, Number) ->
    case ets:lookup(Name, Number) of
        [] ->
            {error, unknown_worker_number};
        [{Number, Id}] ->
            Id
    end.

child_spec(Id, WorkerMod) ->
    {Id,
     {queue_workers_ets_worker, start_link, [Id, WorkerMod]},
     permanent, 
     5000, 
     worker,
     [queue_workers_ets_worker]
    }.