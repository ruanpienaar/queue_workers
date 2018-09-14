-module(queue_workers_ets_work_coordinator).

-export([start_link/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(EtsSupHeirName, TblName, WorkerCount) ->
    gen_server:start_link(?MODULE, {EtsSupHeirName, TblName, WorkerCount}, []).

init({EtsSupHeirName, TblName, WorkerCount}) ->
    true = queue_workers_notify:subscribe(TblName),
    % Round robbin at first...
    {ok, #{
        ets_sup_heir_name => EtsSupHeirName,
        tblname => TblName,
        worker_count => WorkerCount, 
        worker_index => 1
    }}.

handle_call(Request, _From, State) ->
    io:format("~p handle_call ~p\n", [?MODULE, Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("~p handle_cast ~p\n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(new_job, #{ ets_sup_heir_name := EtsSupHeirName,
                        tblname := TblName,
                        worker_count := WC,
                        worker_index := WI } = State) ->
    % Ans = WorkerMod:run_job(Job),
    % io:format("Job Response ~p\n", [Ans]),
    Worker = queue_workers_ets_sup:worker_name(EtsSupHeirName, WI),
    case queue_workers_ets_tbl:take_first(TblName) of
        [] ->
            % why did we get a notification of a new entry,
            % but there's no 'new' ( processing = false ) entry ?
            ok;
        Job ->
            ok = gen_server:call(Worker, {new_job, Job})
    end,
    {noreply, case WI >= WC of
        true ->
            State#{worker_index => 1};
        false ->
            State#{worker_index => WI + 1}
    end};
handle_info(Info, State) ->
    io:format("~p handle_info ~p\n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.