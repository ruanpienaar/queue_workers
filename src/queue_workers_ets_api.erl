-module(queue_workers_ets_api).
-export([
	queue_job/2
]).

queue_job(TblName, Job) ->
    queue_workers_ets_tbl:create(TblName, Job).