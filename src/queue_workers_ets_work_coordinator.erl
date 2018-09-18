-module(queue_workers_ets_work_coordinator).

-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(TblName, WorkerCount) ->
    gen_server:start_link({local, TblName}, ?MODULE, {TblName, WorkerCount}, []).

init({TblName, WorkerCount}) ->
    process_flag(trap_exit, true),
    true = queue_workers_notify:subscribe(TblName),
    Tid = ets:new(queue_workers_ets_work_coordinator, [private]),
    % Round robbin at first
    {ok, #{
        job_table => Tid,
        tblname => TblName,
        worker_count => WorkerCount,
        worker_index => 1
    }}.

handle_call({run_job, Job}, From, #{ tblname := TblName,
                                     worker_index := WI } = State) ->
    Worker = queue_workers_ets_sup:worker_name(TblName, WI),
    % create the job
    {ok, Key} = queue_workers_ets_tbl:create(TblName, Job),
    ok = gen_server:cast(Worker, {sync_new_job, TblName, From, Key, Job}),
    noreply_rotate_worker_index(State);
handle_call(Request, _From, State) ->
    io:format("~p handle_call ~p\n", [?MODULE, Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("~p handle_cast ~p\n", [?MODULE, Msg]),
    {noreply, State}.

handle_info({sync_new_job_results, TblName, From, Key, JobResult}, State) ->
    % Delete the job
    Job = queue_workers_ets_tbl:take(TblName, Key),
    gen_server:reply(From, JobResult),
    {noreply, State};
handle_info(new_job, #{ job_table := Tid,
                        tblname := TblName,
                        worker_index := WI } = State) ->
    WorkerLinkedPid = spawn_link(fun() -> await_commence(TblName, WI) end),
    true = ets:insert(Tid, {WorkerLinkedPid, undefined}),
    WorkerLinkedPid ! commence,
    noreply_rotate_worker_index(State);
handle_info({'EXIT', Pid, normal}, #{ job_table := Tid,
                                      tblname := TblName } = State) ->
    case ets:take(Tid, Pid) of
        [] ->
            io:format("!!!!!! {'EXIT', ~p, normal} not in job table !!!!!!\n", [Pid]);
        [{Pid, undefined}] ->
            queue_workers_ets_tbl:check_first(TblName)
    end,
    {noreply, State};
handle_info(Info, State) ->
    io:format("~p handle_info ~p\n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

noreply_rotate_worker_index(#{ worker_count := WC,
                               worker_index := WI } = State) ->
    {noreply, case WI >= WC of
        true ->
            State#{worker_index => 1};
        false ->
            State#{worker_index => WI + 1}
    end}.

await_commence(TblName, WI) ->
    receive
        commence ->
            % Ans = WorkerMod:run_job(Job),
            % io:format("Job Response ~p\n", [Ans]),
            Worker = queue_workers_ets_sup:worker_name(TblName, WI),
            case queue_workers_ets_tbl:take_first(TblName) of
                [] ->
                    % why did we get a notification of a new entry,
                    % but there's no 'new' ( processing = false ) entry ?
                    ok;
                [{Key, Job}] ->
                    % timeouts will create exceptions
                    gen_server:call(Worker, {new_job, Job})
            end
    end.