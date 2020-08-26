-module(puck_ws).
-export([init/2]).
-export([
         websocket_init/1, websocket_handle/2,
         websocket_info/2
        ]).
-export([ws_json/1]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts,
     #{ idle_timeout => 7200000 }
    }.

websocket_init(_State) ->
    logger:debug("init websocket"),
    {ok, 100}.

websocket_handle({text, <<"ping">>}, State) ->
    {ok, State};

websocket_handle({text, Msg}, State) ->
  try
    JSON=jsx:decode(Msg, [return_maps]),
    logger:info("WS ~p", [JSON]),
    ws_json_reply(JSON, State)
  catch Ec:Ee ->
          logger:error("WS error ~p:~p ~p", [Ec,Ee,Msg]),
          {reply, {text,
                   jsx:encode(#{ ok=>false, error=><<"WAT?">> })
                  }, State }
  end;

websocket_handle(_Any, State) ->
    {reply, {text, << "whut?">>}, State}.

websocket_info({json, JSON}, State) ->
  ws_json_reply(JSON, State);

websocket_info({message, Msg}, State) ->
    logger:info("websocket message ~p", [Msg]),
    {reply, {text, Msg}, State};

websocket_info({timeout, _Ref, Msg}, State) ->
    {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
    io:format("websocket info ~p~n", [_Info]),
    {ok, State}.

ws_json_reply(JSON, State) ->
  case ws_json(JSON) of
    {json, Text} ->
      {reply, {text, jsx:encode(Text)}, State}
  end.

ws_json([<<"watch">>, ID|Args]) ->
  Send=case lists:member(<<"changes">>,Args) of
         false -> full;
         true -> changes
       end,
  gen_server:cast(puck_ws_dispatcher,
                  {subscribe, object, ID, self(), Send}
                 ),
  case lists:member(<<"preload">>,Args) of
    true ->
      self() ! {json, [<<"get">>, ID]};
    _ -> ok
  end,
  {json, #{ ok=>true, sub_id=>ID }};

ws_json([<<"get">>, ID]) ->
  R=case gen_server:call(puck_db,{get,ID}) of
      not_found -> #{ ok=>false, res=>not_found };
      {ok, Data} ->
        #{ ok=>true, res=>Data }
    end,
  {json, R};

ws_json([<<"put">>, ID, Data]) ->
  {ok,_}=gen_server:call(puck_db,{put,ID,Data}),
  {json, <<"OK">>};

ws_json([<<"replace">>, ID, Data]) ->
  {ok,_}=gen_server:call(puck_db,{replace,ID,Data}),
  {json, <<"OK">>};

ws_json([<<"del">>, ID]) ->
  ok=gen_server:call(puck_db,{delete,ID}),
  {json, <<"OK">>};

ws_json(_) ->
  {json, jsx:encode(#{ ok=>false, error=><<"unhandled">> })}.

