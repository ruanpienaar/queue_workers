-module(example_ets_job_worker).
-export([run_job/1]).

% Worker mod
% Make into behaviour

run_job({Key, Value}) ->
    math:cos(Key),
    math:sin(Key),
    math:tan(Key),
    timer:sleep(125),
    % io:format("JOB PAYLOAD [~p]~n", [Payload]).
    ok.