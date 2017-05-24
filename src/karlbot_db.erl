-module(karlbot_db).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get/1, put/2]).

-record(state, {database}).

%%%===================================================================
%%% API functions
%%%===================================================================
%%%
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, DB} = esqlite3:open(code:priv_dir(karlbot) ++ "/karlbot.db"),
    check_db(DB),
    {ok, #state{database=DB}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Key}, _From, State) ->
    Data = karlbot_db_get(Key, State#state.database),
    {reply,Data, State};

handle_call({put, Key, Value}, _From, State) ->
    Reply = karlbot_db_put(Key, Value, State#state.database),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

install_db([], DB) ->
    esqlite3:exec("CREATE TABLE karlbot_data(key text PRIMARY KEY, value blob);",DB);

install_db(_, _) ->
    ok.

check_db(DB) ->
    %% check our table exists, if it doesnt install it
    DbExists = esqlite3:q("SELECT name FROM sqlite_master WHERE type='table' AND name='karlbot_data';",DB),
    install_db(DbExists, DB).


return_get_data([{Data}]) ->
    binary_to_term(Data);
return_get_data(_) ->
    [].

karlbot_db_get(Key, DB) ->
    Data = esqlite3:q("SELECT value FROM karlbot_data WHERE key = ?;", [Key], DB),
    return_get_data(Data).

karlbot_db_put(Key, Value, DB) ->
    esqlite3:exec("INSERT OR REPLACE INTO karlbot_data (key, value) values (?,?);", [Key,term_to_binary(Value)], DB).
