-module(queue_workers_ets_api).
-export([
    queue_job/2,
    run_job/2
]).

%% @doc Queue a job for running at some point in time, with no reply needed.
%% @end
queue_job(TblName, Job) ->
    queue_workers_ets_tbl:create_and_notify(TblName, Job).

%% @doc run_job calls the queue_workers_ets_work_coordinator,
%%      where the job will be queued and the client will wait for the response/timeout
%% @end
run_job(TblName, Job) ->
    true = queue_workers_ets_tbl:create(TblName, Job),
    gen_server:call(TblName, {run_job, Job}).