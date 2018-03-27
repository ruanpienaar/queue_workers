# queue_workers
A generic workers library, where jobs are read from a list of configured sources.

Supported Job configurable sources:

- Erlang ETS ( WIP )

Async job execution:
```Erlang
queue_workers_ets_worker:add_job(value).
```

Sync job with reply:
```Erlang
queue_workers_ets_worker:do_job(Value).
```

- RAbbitMq ( WIP )
- Mnesia ( WIP )
- Sqlite ( WIP )
- SQL over ODBC ( WIP )
