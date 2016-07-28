%%%-------------------------------------------------------------------
%% @doc karlbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(karlbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecs = [
        ?CHILD(slack_client, worker),
        ?CHILD(plugin_manager, worker),
        ?CHILD(slack_pinger, worker)
    ],       
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
