-module(ws_client).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         start_link/1,
         send_text/2,
         recv/2,
         recv/1,
         stop/1
        ]).

-export([
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-record(state, {
          buffer = [] :: list(),
          waiting = undefined :: undefined | pid()
         }).

start_link() ->
    ct:pal(" ---- start_link", []),
    start_link("ws://localhost:8983/websocket").

start_link(Url) ->
    websocket_client:start_link(Url, ?MODULE, []).

stop(Pid) ->
    ct:pal(" ---- stop", []),
    Pid ! stop.

send_text(Pid, Msg) ->
    ct:pal(" ---- send_text", []),
    websocket_client:cast(Pid, {text, Msg}).

recv(Pid) ->
    recv(Pid, 5000).

recv(Pid, Timeout) ->
    Pid ! {recv, self()},
    receive
        M -> M
    after
        Timeout -> error
    end.

init(_, _WSReq) ->
    ct:pal(" ---- init", []),
    {ok, #state{}}.

websocket_handle(Frame, _, State = #state{waiting = undefined, buffer = Buffer}) ->
    ct:pal("Client received frame", []),
    {ok, State#state{buffer = [Frame|Buffer]}};
websocket_handle(Frame, _, State = #state{waiting = From}) ->
    ct:pal("Client received frame~n", []),
    From ! Frame,
    {ok, State#state{waiting = undefined}}.

websocket_info({send_text, Text}, WSReq, State) ->
    ct:pal("info:text", []),
    websocket_client:send({text, Text}, WSReq),
    {ok, State};
websocket_info({recv, From}, _, State = #state{buffer = []}) ->
    ct:pal("info:recv", []),
    {ok, State#state{waiting = From}};
websocket_info({recv, From}, _, State = #state{buffer = [Top|Rest]}) ->
    ct:pal("info:recv", []),
    From ! Top,
    {ok, State#state{buffer = Rest}};
websocket_info(stop, _, State) ->
    ct:pal("info:stop", []),
    {close, <<>>, State}.

websocket_terminate(Close, _, State) ->
    ct:pal("Websocket closed with frame ~p and state ~p", [Close, State]),
    ok.
