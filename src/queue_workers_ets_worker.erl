-module(queue_workers_ets_worker).

-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link(Name, WorkerMod) ->
    gen_server:start_link({local, Name}, ?MODULE, {WorkerMod}, []).

init({WorkerMod}) ->
    {ok, #{ worker_mod => WorkerMod }}.

handle_call({new_job, Job}, _From, #{ worker_mod := WorkerMod } = State) ->
    % io:format("Worker ~p doing Job ~p\n", [self(), Job]),
    {reply, WorkerMod:run_job(Job), State};
handle_call(Request, _From, State) ->
    io:format("~p handle_call ~p\n", [?MODULE, Request]),
    {reply, {error, unknown_call}, State}.

handle_cast({sync_new_job, TblName, From, Key, Job}, #{ worker_mod := WorkerMod } = State) ->
    JobResult = WorkerMod:run_job(Job),
    % gen_server:cast(TblName, {sync_new_job_results, TblName, From, JobResult}),
    TblName ! {sync_new_job_results, TblName, From, Key, JobResult},
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("~p handle_cast ~p\n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("~p handle_info ~p\n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% -export([
%     start_link/3
%     % add_job/1,
%     % do_job/1
% ]).

% % % ------------------------------------

% start_link(Id, Tbl, WorkerMod) ->
%     SupPid = self(),
%     {ok, proc_lib:spawn_link(
%         fun() -> init(Id, Tbl, WorkerMod, SupPid) end
%     )}.

% % %% @doc - Async way of performing work
% % %% @end
% % add_job(Value) ->
% %     Key = erlang:unique_integer([monotonic]),
% %     example_ets_job_table:create(Key, Value).

% % % @doc - Sync way of performing work, falls back to async if no idle workers.
% % % @end
% % do_job(Value) ->
% %     % TODO: mmm, do we want to store the job somewhere? for if it fails ?
% %     Key = erlang:unique_integer([monotonic]),
% %     case lep_load_spread:next_producer_pid() of
% %         Pid when is_pid(Pid) ->
% %             io:format("[~p] self() == ~p~n", [self(), ?MODULE]),
% %             Pid ! {self(), new_job, example_ets_job_table:new(Key, Value)},
% %             receive
% %                 {response, Response} ->
% %                     Response
% %             after
% %                 5000 ->
% %                     {error, timeout}
% %             end;
% %         false ->
% %             add_job(Value),
% %             {ok, request_queued}
% %     end.

% % % ------------------------------------

% init(Id, Tbl, WorkerMod, SupPid) ->
% %     % Check if worker mod has function exported
% %     % io:format("[~p] init ~p~n", [Id, self()]),
%      true = erlang:register(Id, self()),
%      process_flag(trap_exit, true),
% %     % lep_load_spread:add_producer_pid(self()),
% %     % queue_workers_notify:subscribe({queue_workers_ets_worker, self()}),
% %     % ok = Tbl:check_first(),
%      loop(#{ tbl => Tbl, worker_mod => WorkerMod, sup_pid => SupPid }).

% loop(#{ tbl := Tbl, worker_mod := WorkerMod, sup_pid := SupPid } = State) ->
%     receive
%         {ReqPid, new_job, Job} ->
%             ReqPid ! {response, WorkerMod:run_job(Job)},
%             lep_load_spread:idle(self()),
%             loop(State);
%         new_job ->
%             case Tbl:take_first() of
%                 [] ->
%                     lep_load_spread:idle(self());
%                 [Job] ->
%                     Ans = WorkerMod:run_job(Job),
%                     io:format("Job Response ~p\n", [Ans])
%             end,
%             loop(State);
%         %
%         % {job_failed, Job} ->
%         %     ok;
%         % {'EXIT', WPid, normal} ->
%         %     loop(#{ tbl := Tbl, worker_mod := WorkerMod, worker_pid := undefined });
%         {'EXIT', SupPid, shutdown} ->
%             % terminate goes here...

%             ok;
%         X ->
%             io:format("[~p] Unknown Message ~p~n", [?MODULE, X]),
%             loop(State)
%     % Maybe not , maybe needed ??
%     % after
%     %     5 ->
%     %         lep_load_spread:idle(Pid)
%     end.