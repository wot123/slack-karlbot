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

-export([send_message/2, get_team_data/0, find_channel/1]).

-record(state, {pid, ws_pid, teamdata, token, message_id}).

%%%===================================================================
%%% API functions
%%%===================================================================

find_channel(Name) ->
    gen_server:call(?MODULE, {find_channel, Name}).

send_message(Channel, Message) ->
    gen_server:cast(?MODULE, {send_message, Channel, Message}).

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
    {ok, [{token, Token}]} = file:consult('priv/slack.config'),
    {ok, Pid} = gun:open("slack.com", 443),
    StreamRef = gun:get(Pid, "/api/rtm.start?token=" ++ Token),
    {ok, Body} = gun:await_body(Pid, StreamRef, 10000),
    TeamData = jsx:decode(Body),
    WsPid = make_ws_connection(proplists:get_value(<<"url">>,TeamData, false)),
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
handle_info({gun_ws, _Pid, {text, Text}}, State) ->
    Json = jsx:decode(Text),
    syn:publish(slack_messages, Json),
    {noreply, State};

handle_info(Info, State) ->
    lager:info("info: ~p", [Info]),
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

make_ws_connection(false) ->
    0;

make_ws_connection(Uri) ->
    lager:info("url: ~p",[Uri]),
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
                       {<<"text">>, list_to_binary(Message)}]),
    gun:ws_send(State#state.ws_pid, {text, Json}).
