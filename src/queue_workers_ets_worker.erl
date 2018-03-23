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
    io:format("[~p] init ~p~n", [Id, self()]),
    true = erlang:register(Id, self()),
    process_flag(trap_exit, true),
    queue_workers_notify:subscribe(ets_jobs),
    ok = Tbl:check_first(),
    loop(Tbl, WorkerMod).

loop(Tbl, WorkerMod) ->
    receive
        new_job ->
            case Tbl:take_first() of
                [] ->
                    ok;
                [Job] ->
                    ok = WorkerMod:run_job(Job)
            end,
            loop(Tbl, WorkerMod);
        X ->
            io:format("[~p] Unknown Message ~p~n", [?MODULE, X]),
            loop(Tbl, WorkerMod)
    % after
    %    100 ->
    % Set the worker state to idle
    end.