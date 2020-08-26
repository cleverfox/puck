-module(puck_db).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  {ok, DBH} = cowdb:open("puck.db"),
  {ok, #{
     dbh => DBH
    }
  }.

handle_call({get, ID}, _From, #{dbh:=DBH}=State) ->
  Res=case cowdb:get(DBH, ID) of
        {ok, {ID, Data}} ->
          {ok, Data};
        not_found ->
          not_found
      end,
  {reply, Res, State};

handle_call({delete, ID}, _From, #{dbh:=DBH}=State) ->
  {ok,_}=cowdb:delete(DBH, ID),
  gen_server:cast(puck_ws_dispatcher, {obj_changed, ID, delete}),
  {reply, ok, State};

handle_call({replace, ID, Data}, _From, #{dbh:=DBH}=State) when is_map(Data) ->
  {ok,_}=cowdb:put(DBH, ID, Data),
  gen_server:cast(puck_ws_dispatcher, {obj_changed, ID, all}),
  {reply, ok, State};

handle_call({put, ID, Data}, _From, #{dbh:=DBH}=State) when is_map(Data) ->
  Prev=case cowdb:get(DBH, ID) of
         {ok,{ID,#{}=R}} -> R;
         not_found -> #{}
       end,
  Changes=maps:fold(
            fun(K,V,A) ->
                case maps:get(K,Prev,it_is_should_be_really_undefined___)==V of
                  true ->
                    A;
                  false ->
                    [K|A]
                end
            end, [], Data),
  {ok,_}=cowdb:put(DBH, ID, maps:merge(Prev,Data)),
  if Changes == [] -> ok;
     true ->
       gen_server:cast(puck_ws_dispatcher, {obj_changed, ID, Changes})
  end,
  {reply, {ok,Changes}, State};

handle_call(_Request, _From, State) ->
    {reply, unhandled_call, State}.

handle_cast(_Info, State) ->
  logger:info("Unknown CAST ~p", [_Info]),
  {noreply, State}.

handle_info(_Info, State) ->
  lager:info("Unknown INFO ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

