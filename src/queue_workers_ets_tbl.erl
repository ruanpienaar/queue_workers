-module(queue_workers_ets_tbl).

-include_lib("stdlib/include/qlc.hrl").

-export([
    init/1,
    % take_and_stamp_first/1,
    take_first/1,
    check_first/1,
    create/2,
    read/2,
    % update/3,
    delete/2,
    all/1
]).

init(TblName) ->
    TblName = ets:new(TblName, [named_table, ordered_set, public]),
    ok.

take_first(TblName) ->
    case ets:first(TblName) of
        '$end_of_table' ->
            [];
        F ->
            ets:take(TblName, F)
    end.

% Let's keep this later, to keep track of long running procs.
% take_and_stamp_first(TblName) ->
%     % QH = ets:table(TblName),
%     QH = qlc:q([X || X <- ets:table(TblName), element(3, X) == false ]),
%     Cur = qlc:cursor(QH),
%     case qlc:next_answers(Cur, 1) of
%         [] ->
%             ok = qlc:delete_cursor(Cur),
%             [];
%         [{Key, Job, false}] ->
%             true = ets:insert(TblName, {Key, Job, true}),
%             ok = qlc:delete_cursor(Cur),
%             Job
%     end.

% Used when a worker restarts
check_first(TblName) ->
    case ets:first(TblName) of
        '$end_of_table' ->
            ok;
        _Key ->
            queue_workers_notify:publish(TblName, new_job),
            ok
    end.

create(TblName, Job) ->
    Obj = {
        erlang:unique_integer([monotonic]),
        Job,
        false
    },
    case ets:insert(TblName, Obj) of
        true ->
            queue_workers_notify:publish(TblName, new_job),
            true;
        _ ->
            error
    end.

read(TblName, Key) ->
    ets:lookup(TblName, Key).

% update(TblName, Key, Job) ->
%     create(TblName, Key, Job).

delete(TblName, Key) ->
    ets:delete(TblName, Key).

all(TblName) ->
    ets:tab2list(TblName).