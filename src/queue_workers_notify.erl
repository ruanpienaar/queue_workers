-module(queue_workers_notify).

% Notify is used to instruct the workers to start picking up jobs

-export([
         subscribe/1,
         publish/2
]).

publish(queue_workers_ets_worker, Data) ->
    % round robin on idle workers
    % NB: next_producer also set's that pid to busy
    case lep_load_spread:next_producer_pid() of
        Pid when is_pid(Pid) ->
            % io:format("~p~n", [Pid]),
            publish({queue_workers_ets_worker, Pid}, Data);
        false ->
            % io:format("~p~n", [false]),
            % All workers already busy
            {error, no_producers}
    end;
publish(Topic, Data) ->
    % Implement a round robin
    % Is worker already notified ?
    % check if queue_workers_ets_worker state
    gproc:send({p, l, Topic}, Data).

% subscribe({queue_workers_ets_worker, Pid})
subscribe(Topic) ->
    gproc:reg({p, l, Topic}).