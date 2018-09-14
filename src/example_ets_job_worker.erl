-module(example_ets_job_worker).

-behaviour(queue_workers_worker).
-export([
    run_job/1
]).

% Worker mod
% Make into behaviour

run_job(Value) ->
    % timer:sleep(250),
    io:format("JOB PAYLOAD [~p]~n", [Value]),
    ok.