-module(queue_workers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, DataSources} = application:get_env(queue_workers, data_sources),
    Children = lists:foldl(fun
        ({Type = ets, [{Name, Opts}]}, Acc) ->
            append_if_valid(Name, Type, Opts, Acc);
        ({_Type = rabbitmq, [{_Name, _Opts}]}, Acc) ->
            % append_if_valid(Name, Type, Opts, Acc)
            Acc
    end, [], DataSources),
    {ok, { {one_for_one, 5, 10}, Children} }.

append_if_valid(Name, Type, Opts, Acc) ->
    case valid_opts(ets, Opts) of
        true ->
            [ ?CHILD(queue_workers_ets_sup, supervisor, [Name, Opts]) | Acc ];

        false ->
            io:format("Dropped ~p spec for Type ~p Opts ~p\n", 
                [Name, Type, Opts]),
            Acc
    end.

valid_opts(Type, Opts) ->
    ReqOpts = required_opts_per_type(Type),
    lists:all(fun(O) -> lists:keyfind(O, 1, Opts) =/= false end, ReqOpts).

required_opts_per_type(ets) ->
    [table_name, worker_module, worker_count].