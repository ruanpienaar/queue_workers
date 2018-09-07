-module(example_ets_api).

%% API
-export([
    add_job/1
]).

%% Bench
-export([
    sync_bench/0
    % bench/1,
    % add_job/1
]).

%% API
add_job(Job) ->
    queue_worker_ets_worker:add_job(Job).

%% Bench

% These were very basic calculated times with a damn real life stop watch :D
% example_ets_job_worker slept for 125 Ms

% Commit 1
% 7 Min 7 Seconds ( 100 workers )

% Commit 2
% 15.93 Seconds ( 1000 workers )

% bench() ->
%     % N = erlang:system_time(nano_seconds),
%     % [example_ets_api:add_job(test) || _X <- lists:seq(1, 100) ].
%     spawn(fun() -> start_adding(1000000) end),
%     % N2 = erlang:system_time(nano_seconds),
%     % Dur = (N2-N)/1000000000
%     % io:format("======================~nReport:~nDurartion ~p Seconds~n", [Dur]).
%     ok.

sync_bench() ->
    spawn(fun() -> start_doing(10) end).

% bench(A) ->
%     % N = erlang:system_time(nano_seconds),
%     % [example_ets_api:add_job(test) || _X <- lists:seq(1, 100) ].
%     spawn(fun() -> start_adding(A) end),
%     % N2 = erlang:system_time(nano_seconds),
%     % Dur = (N2-N)/1000000000
%     % io:format("======================~nReport:~nDurartion ~p Seconds~n", [Dur]).
%     ok.

% 1521822827860033097
% 1521822827.8600330353

start_doing(0) ->
    io:format("! Done ! ~n", []);
start_doing(C) ->
    % io:format("REPLY : ~p~n", [queue_workers_ets_worker:do_job(C)]),
    % With Pid ( self() is not coded nicely...)
    % spawn(fun() ->
    %     io:format("Do Job (~p) ~n", [self()]),
    %     A = queue_workers_ets_worker:do_job(C),
    %     io:format("Do Job result ~p~n", [A])
    % end),
    % Without pid
    queue_workers_ets_worker:do_job(C),
    start_doing(C-1).

% start_adding(0) ->
%     ok;
% start_adding(C) ->
%     add_job(C),
%     start_adding(C-1).

% add_job(Value) ->
%     Key = erlang:unique_integer([monotonic]),
%     example_ets_job_table:create(Key, Value).