%% -*- coding: utf-8 -*-
-module(naviws_handler).
% -behaviour(cowboy_websocket_handler).

-export([init/2]).
% -export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
% -export([websocket_terminate/3]).

-record(state, {
    subscribe_backend :: atom()
}).

init(Req, [SubscribeBackend]) ->
    SubscribeBackend:watch(self()),
    {cowboy_websocket, Req, #state{subscribe_backend = SubscribeBackend}}.

websocket_handle({text, Msg}, Req, State) ->
    request(jsxn:decode(Msg, [{error_handler, fun(_, _, _) -> {error, badarg} end}]), Req, State);

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

% Сюда поступают сообщения от подписанных ресурсов
websocket_info({json, Data}, Req, State) ->
    Message = jsxn:encode(Data),
    {reply, {text, Message}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

% Private

request({error, badarg}, Req, State) ->
    {reply, {text, << "{\"error\": \"badarg\"}" >>}, Req, State};

request(#{<<"subscribe">> := Subscribes}, Req, #state{subscribe_backend = SubscribeBackend} = State) ->
    Keys = [ tokey(Rec) || Rec <- Subscribes ],
    SubscribeBackend:subscribe(self(), Keys),
    {ok, Req, State};

request(_Any, Req, State) ->
    {reply, {text, << "{\"error\": \"badrequest\"}" >>}, Req, State}.

tokey(#{<<"resource">> := Resource, <<"id">> := Id}) ->
    <<Resource/binary, ":", Id/binary>>;

tokey(_) ->
    <<"no_name:no_id">>.
