-module(example_ets_job_table).

% Db Mod
% Make Db mod into behaviour.

-export([
    init/0,
    take_first/0,
    check_first/0,
    new/2,
    create/2,
    read/1,
    update/2,
    delete/1,
    all/0
]).

init() ->
    ?MODULE = ets:new(?MODULE, [named_table, ordered_set, public]),
    ok.

take_first() ->
    ets:take(?MODULE, ets:first(?MODULE)).

% Used when a worker restarts
check_first() ->
    case ets:first(?MODULE) of
        '$end_of_table' ->
            ok;
        _Key ->
            queue_workers_notify:publish(queue_workers_ets_worker, new_job),
            ok
    end.

new(Key, Value) ->
    {Key, Value}.

create(Key, Value) ->
    case ets:insert(?MODULE, {Key, Value}) of
        true ->
            queue_workers_notify:publish(queue_workers_ets_worker, new_job),
            true;
        _ ->
            error
    end.

read(Key) ->
    ets:lookup(?MODULE, Key).

update(Key, Value) ->
    create(Key, Value).

delete(Key) ->
    ets:delete(?MODULE, Key).

all() ->
    ets:tab2list(?MODULE).
