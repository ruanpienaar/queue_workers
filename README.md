# queue_workers
A generic workers library, where jobs are read from a list of configured sources.

Supported Job configurable sources:

- Erlang ETS ( WIP )

```
example_ets_api:add_job(value).
```
runs:
```
example_ets_job_table:create(Key, Value).
```

Bench run:
This will create 1Mil entries/jobs.
```
example_ets_api:bench().
```

- RAbbitMq ( WIP )
- Mnesia ( WIP )
- Sqlite ( WIP )
- SQL over ODBC ( WIP )
