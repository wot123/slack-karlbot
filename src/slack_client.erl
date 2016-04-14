-module(slack_client).

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

-export([send_file/3, send_message/2,send_term/2, get_team_data/0, find_channel/1, reload/1]).

-record(state, {pid, ws_pid, teamdata, token, message_id, my_user_id, my_handle}).


-define(HELPTEXT,"```Karlbot core commands: \n" ++
                 "list modules\n" ++
                 "start module\n" ++
                 "stop module\n" ++
                 "restart module\n" ++
                 "reload module\n```").
        

%%%===================================================================
%%% API functions
%%%===================================================================
reload(Module) ->
    gen_server:call(?MODULE, {reload_module, Module}).

find_channel(Name) ->
    gen_server:call(?MODULE, {find_channel, Name}).

send_file(Channel, Content, Title) ->
    gen_server:cast(?MODULE, {send_file, Channel, Content, Title}).

send_message(Channel, Message) ->
    gen_server:cast(?MODULE, {send_message, Channel, Message}).

send_term(Channel, Term) ->
    gen_server:cast(?MODULE, {send_term, Channel, Term}).

get_team_data() ->
    gen_server:call(?MODULE, {get_team_data}).

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
    {ok, [{token, Token}]} = file:consult(code:priv_dir(karlbot) ++ '/slack.config'),
    {ok, Pid} = gun:open("slack.com", 443),
    StreamRef = gun:get(Pid, "/api/rtm.start?token=" ++ Token),
    {ok, Body} = gun:await_body(Pid, StreamRef, 10000),
    TeamData = jsx:decode(Body),
    WsPid = make_ws_connection(proplists:get_value(<<"url">>,TeamData, false)),
    syn:join(slack_messages, self()),
    {ok, #state{pid=Pid, ws_pid=WsPid, teamdata=TeamData, 
                token=Token, message_id=0}}.


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
handle_call({reload_module, Module}, _From, State) ->
    {ok, Module, Binary} = compile:file(code:priv_dir(karlbot) ++ "/plugins/" ++ atom_to_list(Module) ++ ".erl", binary),
    code:load_binary(Module, "nofile", Binary),
    {reply, ok, State};

handle_call({find_channel, Name}, _From, State) ->
    Id = get_channel_id(State#state.teamdata, Name),
    {reply, Id, State};

handle_call({get_team_data}, _From, State) ->
    {reply, State#state.teamdata, State};

handle_call(Request, _From, State) ->
    lager:info("call: ~p",[Request]),
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
handle_cast({send_file, ChannelId, Content, Title}, State) ->
    web_send_file(ChannelId, Content, Title, State),
    {noreply, State};

handle_cast({send_term, ChannelId, Term}, State) ->
    R= io_lib:format("~p",[Term]),
    ws_send_message(ChannelId, lists:flatten(R), State),
    {noreply, State};

handle_cast({send_message, ChannelId, Message}, State) ->
    ws_send_message(ChannelId, Message, State),
    {noreply, State};

handle_cast(Msg, State) ->
    lager:info("message: ~p", [Msg]),
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
             {<<"user">>,_Userid},
             {<<"text">>, Text},_,_], State) ->
    
    process_command(string:tokens(binary_to_list(Text)," "),ChannelId),
    {noreply, State};

handle_info({gun_ws, _Pid, {text, Text}}, State) ->
    Json = jsx:decode(Text),
    syn:publish(slack_messages, Json),
    {noreply, State};

handle_info(_Info, State) ->
    %lager:info("info: ~p", [Info]),
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

web_send_file(ChannelId, Content, Title, State) ->
    URL =  "/api/files.upload?token=" ++ State#state.token ++ 
           "&channels=" ++ http_uri:encode(binary_to_list(ChannelId)) ++
           "&content=" ++ http_uri:encode(Content) ++
           "&filetype=markdown",
    lager:info("URL: ~p",[URL]),
    StreamRef = gun:get(State#state.pid,URL), 
    R = gun:await_body(State#state.pid, StreamRef),
    lager:info("R: ~p",[R]),
    ok.

make_ws_connection(false) ->
    0;

make_ws_connection(Uri) ->
    {ok, {_, _, Host, _, Url,_}} = http_uri:parse(binary_to_list(Uri),
                    [{scheme_defaults, [{wss, 443}]}]),
    {ok, WsPid} = gun:open(Host, 443),
    gun:ws_upgrade(WsPid,Url),
    WsPid.
            
find_channel([], _) ->
    false;
find_channel([[{<<"id">>,ID},{<<"name">>,Name}|_]|_], Name) ->
    ID;
find_channel([_|T],Name) ->
    find_channel(T,Name).

get_channel_id(Data, Name) ->
    Channels = proplists:get_value(<<"channels">>, Data),
    find_channel(Channels, list_to_binary(Name)).

ws_send_message(ChannelId, Message, State) ->
    Json = jsx:encode([{<<"id">>, State#state.message_id},
                       {<<"type">>, <<"message">>},
                       {<<"channel">>, ChannelId},
                       {<<"mrkdwn">>, true },
                       {<<"text">>, list_to_binary(Message)}]),
    gun:ws_send(State#state.ws_pid, {text, Json}).

get_bot([], _) ->
    false;
get_bot([[{<<"id">>,Id},{<<"deleted">>,_},{<<"name">>,Name}|_]|_], Name) ->
    Id;
get_bot([_|T], Name) ->
    get_bot(T,Name).

find_bot_id(Data, Name) ->
    Bots = proplists:get_value(<<"bots">>,Data),
    get_bot(Bots, Name).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% slack commands
%%


maybe_show_help(["Karl","help"], ChannelId) ->
    slack_client:send_message(ChannelId, ?HELPTEXT);

maybe_show_help(_,_) ->
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maybe_show_modules(["Karl","show","modules"], _ChannelId) ->
    ok;
maybe_show_modules(_,_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_reload_module(["Karl","reload",Text],ChannelId) ->
    Module = list_to_atom(Text),
    {ok, Module, Binary} = compile:file(code:priv_dir(karlbot) ++ "/plugins/" ++ Text ++ ".erl", binary),
    T = code:load_binary(Module, "nofile", Binary),
    slack_client:send_term(ChannelId, T);

maybe_reload_module(_,_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_list_modules(["Karl","list","modules"], ChannelId) ->
    T = supervisor:which_children(module_sup),
    slack_client:send_term(ChannelId, T),
    ok;
maybe_list_modules(_,_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_start_module(["Karl","start",Module], ChannelId) ->
    T = module_sup:start_module(Module,ChannelId),
    slack_client:send_term(ChannelId, T),
    ok;
maybe_start_module(_,_) ->
    ok.

maybe_stop_module(["Karl","stop",Module], ChannelId) ->
    R = module_sup:stop_module(Module),
    slack_client:send_term(ChannelId, R),
    ok;
maybe_stop_module(_,_) ->
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maybe_restart_module(["Karl", "restart", Text], ChannelId) ->
    T = module_sup:stop_module(Text),
    Module = list_to_atom(Text),
    {ok, Module, Binary} = compile:file(code:priv_dir(karlbot) ++ "/plugins/" ++ Text ++ ".erl", binary),
    code:load_binary(Module, "nofile", Binary),
    T = module_sup:start_module(Text, ChannelId),
    slack_client:send_term(ChannelId, T);

maybe_restart_module(_,_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_command(Text,ChannelId) ->
    maybe_show_modules(Text, ChannelId),
    maybe_show_help(Text, ChannelId),
    maybe_restart_module(Text, ChannelId),
    maybe_reload_module(Text, ChannelId),
    maybe_list_modules(Text, ChannelId),
    maybe_start_module(Text, ChannelId),
    maybe_stop_module(Text, ChannelId).

