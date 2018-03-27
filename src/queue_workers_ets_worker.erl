-module(queue_workers_ets_worker).
-export([
    start_link/3,
    add_job/1,
    do_job/1
]).

% ------------------------------------

start_link(Id, Tbl, WorkerMod) ->
    {ok, proc_lib:spawn_link(
        fun() -> init(Id, Tbl, WorkerMod) end
    )}.

% @doc - Async way of performing work
% @end
add_job(Value) ->
    Key = erlang:unique_integer([monotonic]),
    example_ets_job_table:create(Key, Value).

% @doc - Sync way of performing work, falls back to async if no idle workers.
% @end
do_job(Value) ->
    % TODO: mmm, do we want to store the job somewhere? for if it fails ?
    Key = erlang:unique_integer([monotonic]),
    case lep_load_spread:next_producer_pid() of
        Pid when is_pid(Pid) ->
            io:format("[~p] self() == ~p~n", [self(), ?MODULE]),
            Pid ! {self(), new_job, example_ets_job_table:new(Key, Value)},
            receive
                {response, Response} ->
                    Response
            after
                5000 ->
                    {error, timeout}
            end;
        false ->
            add_job(Value),
            {ok, request_queued}
    end.

% ------------------------------------

init(Id, Tbl, WorkerMod) ->
    % Check if worker mod has function exported
    % io:format("[~p] init ~p~n", [Id, self()]),
    true = erlang:register(Id, self()),
    process_flag(trap_exit, true),
    lep_load_spread:add_producer_pid(self()),
    queue_workers_notify:subscribe({queue_workers_ets_worker, self()}),
    ok = Tbl:check_first(),
    loop(#{ tbl => Tbl, worker_mod => WorkerMod }).

loop(#{ tbl := Tbl, worker_mod := WorkerMod } = State) ->
    receive
        {ReqPid, new_job, Job} ->
            ReqPid ! {response, WorkerMod:run_job(Job)},
            lep_load_spread:idle(self()),
            loop(State);
        new_job ->
            case Tbl:take_first() of
                [] ->
                    lep_load_spread:idle(self());
                [Job] ->
                    Ans = WorkerMod:run_job(Job)
            end,
            loop(State);
        %
        % {job_failed, Job} ->
        %     ok;
        % {'EXIT', WPid, normal} ->
        %     loop(#{ tbl := Tbl, worker_mod := WorkerMod, worker_pid := undefined });
        X ->
            io:format("[~p] Unknown Message ~p~n", [?MODULE, X]),
            loop(State)

    % Maybe not , maybe needed ??
    % after
    %     5 ->
    %         lep_load_spread:idle(Pid)
    end.