-module(lep_load_spread).

-define(ADD_P_PID, add_producer_pid).
-define(DEL_P_PID, del_producer_pid).
-define(NEXT_P_PID, next_producer_pid).
-define(ALL_P, all_producers).

-export([
    start_link/0,
    init/1,
    add_producer_pid/1,
    del_producer_pid/1,
    next_producer_pid/0,
    all_producers/0
]).

-export([
    idle/1
]).

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

add_producer_pid(Pid) ->
    req({?ADD_P_PID, Pid}).

del_producer_pid(Pid) ->
    req({?DEL_P_PID, Pid}).

next_producer_pid() ->
    case req(?NEXT_P_PID) of
        '$end_of_table' ->
            false;
        R ->
            R
    end.

all_producers() ->
    req(?ALL_P).

idle(Pid) ->
    req({?ADD_P_PID, Pid}).

%% -----------------------------------------------------------

init(Parent) ->
    ?MODULE = ets:new(?MODULE,
        [named_table, protected, ordered_set, {read_concurrency, true}]),
    loop(#{next_producer => ets:first(?MODULE)}, Parent).

loop(StateMap, Parent) ->
    true = erlang:register(?MODULE, self()),

    % The code below gets the active workers, this is for
    % when this process dies.

    % case whereis(lep_prod_sup) of
    %     undefined -> %% Startup
    %         ok;
    %     _Pid ->      %% Running
    %         lists:foreach(fun({I,PPid,worker,_Mods}) ->
    %             true = insert(object(PPid))
    %         end, supervisor:which_children(lep_prod_sup))
    % end,

    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(StateMap).

loop(#{ next_producer := Next } = StateMap) ->
    receive
        {req, {?ADD_P_PID, PPid}, RespPid} ->
            true = insert(object(PPid)),
            RespPid ! {response, true},
            loop(StateMap);
        {req, {?DEL_P_PID, PPid}, RespPid} ->
            true = ets:delete(?MODULE, PPid),
            RespPid ! {response, true},
            loop(StateMap);
        {req, ?NEXT_P_PID, RespPid} ->
            NewNext = ets_next(ets:info(?MODULE, size), Next),
            RespPid ! {response, NewNext},
            loop(StateMap#{ next_producer => NewNext });
        {req, ?ALL_P, RespPid} ->
            RespPid ! {response, ets:tab2list(?MODULE)},
            loop(StateMap);
        R ->
            io:format("eh? ~p ~p ~p", [?MODULE, ?LINE, R]),
            loop(StateMap)
    end.

req(Req) ->
    ?MODULE ! {req, Req, self()},
    receive
        {response, Resp} ->
            Resp;
        X ->
            io:format("eh? ~p ~p ~p", [?MODULE, ?LINE, X])
        after
            5000 ->
                {error, timeout}
    end.

ets_next(0, _ ) ->
    false;
ets_next(Attempts, '$end_of_table') ->
    case ets:first(?MODULE) of
        '$end_of_table' ->
            false;
        NextKey ->
            case ets:lookup(?MODULE, NextKey) of
                [{NextKey, idle}] ->
                    true = ets:insert(?MODULE, {NextKey, busy}),
                    NextKey;
                [{NextKey, busy}] ->
                    ets_next(Attempts-1, NextKey)
            end
    end;
ets_next(Attempts, Next) ->
    case ets:next(?MODULE, Next) of
        '$end_of_table' ->
            ets_next(Attempts, '$end_of_table');
        NextKey ->
            case ets:lookup(?MODULE, NextKey) of
                [{NextKey, idle}] ->
                    true = ets:insert(?MODULE, {NextKey, busy}),
                    NextKey;
                [{NextKey, busy}] ->
                    ets_next(Attempts-1, NextKey)
            end
    end.

object(Key) ->
    {Key, idle}.

insert(Obj) ->
    ets:insert(?MODULE, Obj).