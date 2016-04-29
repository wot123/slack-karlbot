-module(plugin_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {self, plugin_dirs}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    {ok,[Config]} = file:consult(code:priv_dir(karlbot) ++ "/plugins.config"),
    Dirs = proplists:get_value(dirs, Config),
    AutoStart = proplists:get_value(auto_start, Config),
    
    gen_server:cast(?MODULE, {auto_start, AutoStart}),
    syn:join(slack_messages, self()),
    {ok, #state{self = slack_client:get_self(), plugin_dirs = Dirs}}.

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
handle_cast({auto_start, StartList}, State) ->
    lager:info("autostart: ~p",[StartList]),
    lists:map(fun(P) -> compile_and_load(P) end, StartList),
    start_plugins(StartList),
    {noreply, State};

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
handle_info([{<<"type">>,<<"message">>},
             {<<"channel">>,ChannelId},
             {<<"user">>, _UserId},
             {<<"text">>, Text},_,_], State) ->
    process_command(string:tokens(binary_to_list(Text)," "),ChannelId,State),
    {noreply, State};

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

list_plugins() ->
    Dirs = lists:filter(fun(D) -> filelib:is_dir(D) end, 
                    filelib:wildcard(code:priv_dir(karlbot) ++ "/*/*/")),
    Filtered = lists:filter(fun(D) -> filename:basename(D) /= ".git" end, Dirs),
    lists:map(fun(D) -> filename:basename(D) end, Filtered).


load_binary(error) ->
    false;
load_binary({ok, Module, Binary}) ->
    code:load_binary(Module, "nofile", Binary).

compile_and_load([]) ->
    false;
compile_and_load(Plugin) ->
    Directory = filelib:wildcard(code:priv_dir(karlbot) ++ "/*/" ++ Plugin ++ "/"),
    lager:info("directory: ~p",[Directory]),
    [Bins] = lists:map(fun(D) -> filelib:fold_files(D,"[a-zA-Z0-9_].erl", true, 
                    fun(F,AccIn) -> [compile:file(F, binary)|AccIn] end, []) end, Directory),
    lists:map(fun(D) -> load_binary(D) end, Bins).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_plugins([]) ->
    ok;
start_plugins([H|T]) ->
    module_sup:start_module(H, false),
    start_plugins(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maybe_list_plugins([Me,"list","plugins"], ChannelId, Me, _State) ->
    lager:info("list plugins"),
    Plugins = list_plugins(),
    slack_client:send_term(ChannelId, Plugins);

maybe_list_plugins(_, _, _, _) ->
    false.

maybe_load_plugin([Me,"load", Plugin], ChannelId, Me, _State) ->
    slack_client:send_term(ChannelId, compile_and_load(Plugin));

maybe_load_plugin(_, _, _, _) ->
    false.

maybe_start_plugin([Me,"start", Plugin], ChannelId, Me) ->
    module_sup:start_module(Plugin, ChannelId);
maybe_start_plugin(_,_,_) ->
    false.

maybe_stop_plugin([Me,"stop", Plugin], ChannelId, Me) ->
    slack_client:send_term(ChannelId, module_sup:stop_module(Plugin));
maybe_stop_plugin(_,_,_) ->
    false.

maybe_restart_plugin([Me,"restart", Plugin], ChannelId, Me, State) ->
    module_sup:stop_module(Plugin),
    maybe_load_plugin([Me,"load",Plugin], ChannelId, Me, State),
    module_sup:start_module(Plugin, ChannelId);
maybe_restart_plugin(_, _, _, _) ->
    false.

get_handle(#{<<"id">> := Id}) ->
    "\<\@" ++ binary_to_list(Id) ++ "\>:".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_command(Tokens, ChannelId, State) ->
    maybe_list_plugins(Tokens, ChannelId, get_handle(State#state.self), State),
    maybe_load_plugin(Tokens, ChannelId, get_handle(State#state.self), State),
    maybe_start_plugin(Tokens, ChannelId, get_handle(State#state.self)),
    maybe_stop_plugin(Tokens, ChannelId, get_handle(State#state.self)),
    maybe_restart_plugin(Tokens, ChannelId, get_handle(State#state.self), State).

