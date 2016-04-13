-module(module_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_module/2, stop_module/1]).

-define(CHILD(I, Type, Param), {I, {I, start_link, [Param]}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_module(Module, ChannelId) when is_list(Module) ->
    Name = list_to_atom(Module),
    supervisor:start_child(?MODULE, ?CHILD(Name, worker, ChannelId));
start_module(_,_) ->
    false.

stop_module(Module) when is_list(Module) ->
    Name = list_to_atom(Module),
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name);
stop_module(_) ->
    false.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    % ?CHILD('SomeChild', 'SomeModule', worker, [])
    {ok, {{one_for_one, 5, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
