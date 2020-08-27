-module(puck_ws_dispatcher).
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
  {ok, #{
     addrsub=>#{},
     pidsub=>#{},
     txsub=>[]
    }
    }.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({obj_changed, ID, delete}, #{addrsub:=AS}=State) ->
  Subscribers=maps:get(ID,AS,[]),
  TxJSAll=jsx:encode(#{event=><<"obj_ch">>, id=>ID, exists=>false, data=>null}),
  lists:foreach(fun
                  ({Pid, changes}) ->
                    erlang:send(Pid, {message, TxJSAll});
                  ({Pid, full}) ->
                    erlang:send(Pid, {message, TxJSAll})
                end, Subscribers),
  {noreply, State};

handle_cast({obj_changed, ID, Fields}, #{addrsub:=AS}=State) ->
  case maps:is_key(ID,AS) of
    false ->
      done;
    true ->
      Subscribers=maps:get(ID,AS),
      {ok,Data}=gen_server:call(puck_db,{get,ID}),
      TxJSAll=jsx:encode(#{event=><<"obj_ch">>, id=>ID, exists=>true, data=>Data}),
      TxJS=case Fields of
             all ->
               jsx:encode(#{event=><<"obj_ch">>, id=>ID, changed=>Data});
             _ ->
               jsx:encode(#{event=><<"obj_ch">>, id=>ID, changed=>maps:with(Fields,Data)})
           end,
      lists:foreach(fun
                      ({Pid, changes}) ->
                        erlang:send(Pid, {message, TxJS});
                      ({Pid, full}) ->
                        erlang:send(Pid, {message, TxJSAll})
                    end, Subscribers)
  end,
  {noreply, State};

handle_cast({subscribe, object, ID, Pid, Mode}, #{addrsub:=AS, pidsub:=PS}=State) ->
    monitor(process, Pid),
    Subscribed=maps:get(Pid,PS,[]),
    case lists:member({obj,ID},Subscribed) of
      true ->
        {noreply, State};
      false ->
        {noreply, State#{
                    addrsub=>maps:put(ID, [{Pid, Mode}|maps:get(ID, AS, [])], AS),
                    pidsub=>maps:put(Pid, [{obj, ID}|Subscribed], PS)
                   }
        }
    end;

handle_cast(_Info, State) ->
  logger:info("Unknown CAST ~p", [_Info]),
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason},
            #{addrsub:=AS0, pidsub:=PS}=State) ->
  io:format("PID ~p dead ~p~n",[Pid,_Reason]),
  Subs=maps:get(Pid, PS, []),
  AS1=lists:foldl(fun({obj, A}, AS) ->
                      io:format("A ~p~n",[A]),
                        AAS=lists:filter(
                              fun({PP,_}) -> PP=/=Pid
                              end, maps:get(A, AS, [])),
                        if AAS == [] ->
                             maps:remove(A, AS);
                           true ->
                             maps:put(A, AAS, AS)
                        end
                    end, AS0, Subs),
  {noreply, State#{
              addrsub=>AS1,
              pidsub=>maps:remove(Pid, PS)
             }
  };


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

