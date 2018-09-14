-module(queue_workers_worker).

-callback run_job(term()) -> term().