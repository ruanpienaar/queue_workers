-module(example_ets_api).

-export([
    bench/0,
    add_job/1
]).

% These were very basic calculated times with a damn real life stop watch :D
% example_ets_job_worker slept for 125 Ms

% Commit 1
% 7 Min 7 Seconds ( 100 workers )

% Commit 2
% 15.93 Seconds ( 1000 workers )

bench() ->
    % N = erlang:system_time(nano_seconds),
    % [example_ets_api:add_job(test) || _X <- lists:seq(1, 100) ].
    spawn(fun() -> start_adding(1000000) end),
    % N2 = erlang:system_time(nano_seconds),
    % Dur = (N2-N)/1000000000
    % io:format("======================~nReport:~nDurartion ~p Seconds~n", [Dur]).
    ok.

% 1521822827860033097
% 1521822827.8600330353


start_adding(0) ->
    ok;
start_adding(C) ->
    add_job(C),
    start_adding(C-1).

add_job(Value) ->
    Key = erlang:unique_integer([monotonic]),
    example_ets_job_table:create(Key, Value).