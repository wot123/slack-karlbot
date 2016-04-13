%%%-------------------------------------------------------------------
%% @doc karlbot public API
%% @end
%%%-------------------------------------------------------------------

-module(karlbot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    syn:init(),
    module_sup:start_link(),
    karlbot_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
