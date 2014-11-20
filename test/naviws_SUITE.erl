-module(naviws_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

suite() ->
    [{timetrap,{minutes,1}}].

all() -> [ test_1, test_2 ].

init_per_suite(Config) ->
    error_logger:tty(false),
    {ok, Modules} = application:ensure_all_started(naviws),
    % {ok, GunModules} = application:ensure_all_started(gun),
    [{modules, Modules} | Config].

end_per_suite(Config) ->
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(naviws),
    error_logger:tty(true),
    ok.


-define(PORT, 8983).

test_1(_) ->
    {ok, Pid} = ws_client:start_link(),
    % timer:sleep(50),
    ?assertMatch({ok, [_]}, navidb_subs:pids()),    % Один пид
    % Произведем подписку на обновление ресурса
    Message = jsxn:encode(#{
        subscribe => [#{
            resource => <<"account">>,
            id       => <<"baden">>
        }]
    }),
    ws_client:send_text(Pid, Message),
    timer:sleep(50),
    {ok, Listeners} = navidb_subs:listeners(<<"account:baden">>),
    ?assertEqual(1, length(Listeners)),

    % Document = navidb:get(accounts, {username, <<"baden">>}),
    % ?debugFmt(" Document = ~p", [Document]),
    % Установим значение поля
    navidb:set(accounts, {username, <<"baden">>}, #{foo => <<"bar">>}),

    % Websocket-клиент должен получить уведомление
    {text, Response} = ws_client:recv(Pid),
    #{<<"messages">> := [Frame]}  = jsxn:decode(Response),

    ?assertMatch(#{
                    <<"data">>     := #{<<"foo">> := <<"bar">>},
                    <<"id">>       := <<"baden">>,
                    <<"resource">> := <<"account">>
                 }, Frame),
    ws_client:stop(Pid),    % Отключимся
    timer:sleep(50),
    ok.

test_2(_) ->
    ok.
