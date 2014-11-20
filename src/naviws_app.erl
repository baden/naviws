%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%% @copyright Denis Batrak
%% @author Batrak Denis <baden.i.ua@gmail.com>
%% @version {@vsn}, {@date} {@time}
%% @doc ErlNaviCC public API
%% @end
%%%-------------------------------------------------------------------
-module(naviws_app).

-behaviour(application).

-define(APP, naviws).

%% Application callbacks
-export([start_phase/3, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    naviws_sup:start_link().

start_phase(listen, _Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", naviws_handler, []}
        ]}
    ]),

    cowboy:start_http(?APP, config(acceptors, 100),
                      [{port, config(ws_listen_port)}],
                      [{env,
                        [{dispatch, Dispatch}]}]),
    ok.

stop(_State) ->
    ok.

% Private

config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.
