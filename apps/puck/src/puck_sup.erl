%%%-------------------------------------------------------------------
%% @doc puck top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(puck_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 10},
    ChildSpecs = [
                  { ws_dispatcher, {puck_ws_dispatcher, start_link, []},
                    permanent, 5000, worker, []},
                  { puck_db, {puck_db, start_link, []},
                    permanent, 5000, worker, []}
                 
                 
                 ]++ puck_http:childspec(),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
