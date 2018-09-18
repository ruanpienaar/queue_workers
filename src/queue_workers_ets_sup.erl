-module(queue_workers_ets_sup).
-export([start_link/1]).

% The supervisor for all ETS setups

-behaviour(supervisor).
-export([init/1]).

-export([
    sup_name/1,
    worker_name/2
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(Opts) ->
    {table_name, TblName} = lists:keyfind(table_name, 1, Opts),
    SupName = sup_name(TblName),
    supervisor:start_link({local, SupName}, ?MODULE, {SupName, TblName, Opts}).

% Change into simple_one_for_one, and make
% create_child,

init({SupName, TblName, Opts}) ->
    % TODO: create sync and async table.
    %       just do check_first on async table
    %       and build something to check expired jobs
    SupName = ets:new(SupName, [named_table, public, ordered_set]), 

    SupNameSync = ets:new(list_to_atom(atom_to_list(SupName)++"_sync"), [named_table, public, ordered_set]),
    SupNameASync = ets:new(list_to_atom(atom_to_list(SupName)++"_async"), [named_table, public, ordered_set]),

    {worker_count, WorkerCount} = lists:keyfind(worker_count, 1, Opts),
    {worker_module, WorkerMod} = lists:keyfind(worker_module, 1, Opts),
    ok = try_db_init(TblName),
    Children = [
        begin
            Id = list_to_atom(atom_to_list(TblName)++"_worker_"++integer_to_list(X)),
            true = ets:insert(SupName, {{TblName, X}, Id}),
            child_spec(Id, WorkerMod)
        end
        || X <- lists:seq(1, WorkerCount)
    ],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy,
            [ ?CHILD(queue_workers_ets_work_coordinator, worker, [TblName, WorkerCount]) |
              Children ]
         }
    }.

%% @doc Sup is heir of ets table.
%%      So if proc dies, then table stil exists.
%% @end
try_db_init(TblName) ->
    ok = queue_workers_ets_tbl:init(TblName).

worker_name(TblName, Number) ->
    SupName = sup_name(TblName),
    case ets:lookup(SupName, {TblName, Number}) of
        [] ->
            {error, unknown_worker_number};
        [{{TblName, Number}, Id}] ->
            Id
    end.

child_spec(Id, WorkerMod) ->
    Name = Id,
    {Id,
     {queue_workers_ets_worker, start_link, [Name, WorkerMod]},
     permanent,
     5000,
     worker,
     [queue_workers_ets_worker]
    }.

sup_name(TblName) ->
    list_to_atom(atom_to_list(TblName)++"_sup").