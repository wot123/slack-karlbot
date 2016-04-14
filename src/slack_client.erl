-module(slack_client).

-behaviour(gen_fsm).

%% API functions
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1,
         connecting/2,
         connecting/3,
         connected/2,
         connected/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([get_self/0, send_file/3, send_message/2, send_term/2, get_team_data/0, find_channel/1]).

-record(state, {pid, ws_pid, teamdata, token, message_id, self}).

%%%===================================================================
%%% API functions
%%%===================================================================

get_self() ->
    gen_fsm:sync_send_all_state_event(?MODULE, {get_self}).

find_channel(_Channel) ->
    ok.

get_team_data() ->
    ok.

send_message(ChannelId, Message) ->
    gen_fsm:send_event(?MODULE,{send_message, ChannelId, Message}).

send_term(ChannelId, Term) ->
    gen_fsm:send_event(?MODULE,{send_term, ChannelId, Term}).

send_file(_ChannelId, _File, _Title) ->
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, [{token, Token}]} = file:consult(code:priv_dir(karlbot) ++ "/slack.config"),
    {ok, Pid} = gun:open("slack.com", 443),
    StreamRef = gun:get(Pid, "/api/rtm.start?token=" ++ Token),
    {ok, Body} = gun:await_body(Pid, StreamRef, 10000),
    TeamData = jsx:decode(Body),
    WsPid = make_ws_connection(proplists:get_value(<<"url">>, TeamData, false)),

    Self = proplists:get_value(<<"self">>,TeamData),

    {ok, connecting, #state{pid=Pid, ws_pid=WsPid, teamdata=TeamData, 
                           token=Token, message_id=0, self=Self}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
connecting(_Event, State) ->
    {next_state, connecting, State}.

connected({send_term, ChannelId, Term}, State) ->
    R= io_lib:format("~p",[Term]),
    ws_send_message(ChannelId, lists:flatten(R), State),
    {next_state, connected, State};

connected({send_message, ChannelId, Message}, State) ->
    ws_send_message(ChannelId, Message, State),
    {next_state, connected, State};

connected(_Event, State) ->
    {next_state, connected, State}.
%%-------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
connecting(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, connecting, State}.

connected(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, connected, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event({get_self}, _From, StateName, State) ->
    {reply, State#state.self, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({gun_ws, _Pid, {text, Text}}, connecting, State) ->
    Json = jsx:decode(Text),
    case Json of
        [{<<"type">>,<<"hello">>}] ->
            lager:info("connected"),
            {next_state, connected, State};
        _ ->
            {next_state, connecting, State}
    end;

handle_info({gun_ws, _Pid, {text, Text}}, connected, State) ->
    Json = jsx:decode(Text),
    syn:publish(slack_messages, Json),
    {next_state, connected, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ws_send_message(ChannelId, Message, State) ->
    Json = jsx:encode([{<<"id">>, State#state.message_id},
                       {<<"type">>, <<"message">>},
                       {<<"channel">>, ChannelId},
                       {<<"mrkdwn">>, true },
                       {<<"text">>, list_to_binary(Message)}]),
    gun:ws_send(State#state.ws_pid, {text, Json}).


make_ws_connection(false) ->
    0;
make_ws_connection(Uri) ->
    {ok, {_,_, Host, _,Url,_}} = http_uri:parse(binary_to_list(Uri),
                            [{scheme_defaults, [{wss, 443}]}]),
    {ok, WsPid} = gun:open(Host, 443),
    gun:ws_upgrade(WsPid, Url),
    WsPid.
