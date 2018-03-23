-module(example_ets_job_worker).
-export([run_job/1]).

% Worker mod
% Make into behaviour

run_job(_Payload) ->
    timer:sleep(125),
    % io:format("JOB PAYLOAD [~p]~n", [Payload]).
    ok.