-module(puck_httpapi).

-export([h/3, after_filter/1]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

after_filter(Req) ->
  Origin=cowboy_req:header(<<"origin">>, Req, <<"*">>),
  Req1=cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                  Origin, Req),
  Req2=cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                  <<"GET, POST, OPTIONS">>, Req1),
%  Req3=cowboy_req:set_resp_header(<<"access-control-allow-credentials">>,
%                                  <<"true">>, Req2),
  Req4=cowboy_req:set_resp_header(<<"access-control-max-age">>,
                                  <<"86400">>, Req2),
  cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
                             <<"content-type">>, Req4).

h(<<"GET">>, [<<"get">>, _ID]=URL, _Req) ->
  {json, Res} = puck_ws:ws_json(URL),
  {200, Res};

h(<<"POST">>, [<<"put">>, _ID]=URL, Req) ->
  Data=apixiom:bodyjs(Req),
  {json, Res} = puck_ws:ws_json(URL++[Data]),
  {200, Res};

h(<<"POST">>, [<<"replace">>, _ID]=URL, Req) ->
  Data=apixiom:bodyjs(Req),
  {json, Res} = puck_ws:ws_json(URL++[Data]),
  {200, Res};

h(<<"POST">>, [<<"del">>, _ID]=URL, _Req) ->
  {json, Res} = puck_ws:ws_json(URL),
  {200, Res};

h(<<"DELETE">>, [<<"del">>, _ID]=URL, _Req) ->
  {json, Res} = puck_ws:ws_json(URL),
  {200, Res};

h(<<"OPTIONS">>, _, _Req) ->
  {200, [], ""}.

%PRIVATE API

