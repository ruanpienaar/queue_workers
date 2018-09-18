# queue_workers
A generic workers library, where jobs are read from a list of configured sources.

Supported Job configurable sources:

- Erlang ETS ( WIP )

Async job execution: ( use when you don't need a reply, and the order is not required )
```Erlang
queue_workers_ets_worker:add_job(value).
```

Sync job with reply: ( use when a reply and order is required )
```Erlang
queue_workers_ets_worker:do_job(Value).
```

Running from start script:
```
$ ./start-dev.sh
erl> application:ensure_all_started(queue_workers).
```

- RAbbitMq ( WIP )
- Mnesia ( WIP )
- Sqlite ( WIP )
- SQL over ODBC ( WIP )
