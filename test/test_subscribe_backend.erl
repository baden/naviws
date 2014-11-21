-module(test_subscribe_backend).

-export([watch/1, subscribe/2]).

watch(Pid) when is_pid(Pid) ->
    erlang:error(tbd).

subscribe(Pid, Keys) when is_pid(Pid), is_list(Keys) ->
    erlang:error(tbd).
