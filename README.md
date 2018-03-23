# queue_workers
A generic workers library, where jobs are read from a list of configured sources.

Supported Job configurable sources:

- Erlang ETS ( WIP )

```
queue_workers_ets:create(key, value)
```
runs:
```
example_ets_job_worker:run_job({key, value})
```

- RAbbitMq ( WIP )
- Mnesia ( WIP )
- Sqlite ( WIP )
- SQL over ODBC ( WIP )
