-module(queue_workers_ets_worker).
-export([
    start_link/3
]).

% ------------------------------------

start_link(Id, Tbl, WorkerMod) ->
    {ok, proc_lib:spawn_link(
        fun() -> init(Id, Tbl, WorkerMod) end
    )}.

% ------------------------------------

init(Id, Tbl, WorkerMod) ->
    % Check if worker mod has function exported
    % io:format("[~p] init ~p~n", [Id, self()]),
    true = erlang:register(Id, self()),
    process_flag(trap_exit, true),
    lep_load_spread:add_producer_pid(self()),
    queue_workers_notify:subscribe({queue_workers_ets_worker, self()}),
    ok = Tbl:check_first(),
    loop(Tbl, WorkerMod).

loop(Tbl, WorkerMod) ->
    receive
        new_job ->
            case Tbl:take_first() of
                [] ->
                    lep_load_spread:idle(self());
                [Job] ->
                    self() ! new_job,
                    ok = WorkerMod:run_job(Job)
            end,
            loop(Tbl, WorkerMod);
        X ->
            io:format("[~p] Unknown Message ~p~n", [?MODULE, X]),
            loop(Tbl, WorkerMod)

    % Maybe not , maybe needed ??
    % after
    %     5 ->
    %         lep_load_spread:idle(Pid)
    end.