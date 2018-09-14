-module(queue_workers_notify).

% Notify is used to instruct the workers to start picking up jobs

-export([subscribe/1,
         publish/2
]).

publish(Topic, Data) ->
    gproc:send({p, l, Topic}, Data).

subscribe(Topic) ->
    gproc:reg({p, l, Topic}).