-module(puck_http).
-export([childspec/0]).


get_http_conn_type() ->
  HTTPDispatch = cowboy_router:compile(
    [
      {'_', [
        {"/api/ws", puck_ws, []},
        {"/api/[...]", apixiom, {puck_httpapi, #{}}},
        {"/", cowboy_static, {file, "public/index.html" } },
        {"/[...]", cowboy_static,
          {dir, "public",
            [
              {mimetypes, cow_mimetypes, all},
              {dir_handler, directory_handler}
            ]
          }
        }
      ]}
    ]),
  #{
    connection_type => supervisor,
    env => #{
      dispatch => HTTPDispatch
    }
  }.


get_http_opts(Port) ->
  [
    {connection_type, supervisor},
    {port, Port}
  ].


childspec() ->
  Port = application:get_env(tfb, port, 8086),
  HTTPOpts = get_http_opts(Port),
  HTTPConnType = get_http_conn_type(),
  [
    ranch:child_spec(
      http,
      ranch_tcp,
      HTTPOpts,
      cowboy_clear,
      HTTPConnType
    ),
    ranch:child_spec(
      http6,
      ranch_tcp,
      [inet6, {ipv6_v6only, true} | HTTPOpts],
      cowboy_clear,
      HTTPConnType
    )
  ].

