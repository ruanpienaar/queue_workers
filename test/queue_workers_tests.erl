-module(queue_workers_tests).
-include_lib("eunit/include/eunit.hrl").

queue_workers_test_() ->
    {setup,
     % Setup Fixture
     fun() -> 
         xxx
     end,
     % Cleanup Fixture
     fun(xxx) ->
         ok
     end,
     % List of tests
     [
       % Example test
       fun func1/0
     ]
    }.

func1() ->
    ?assert(
        is_list(queue_workers:module_info())
    ).
