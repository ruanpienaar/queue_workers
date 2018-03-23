-module(queue_workers_notify).

-export([subscribe/1,
         publish/2
]).

% this process could subscribe to alive processes.
% once they die, remove them from state

% Topic = {workerPid, Data}
publish(Topic, Data) ->

    % Implement a round robin

    % Is worker already notified ?
    % check if queue_workers_ets_worker state

    gproc:send({p, l, Topic}, Data).

subscribe(Topic) ->
    gproc:reg({p, l, Topic}).