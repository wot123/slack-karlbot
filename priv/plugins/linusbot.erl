-module(linusbot).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-define(STARTUPTEXT, "`linusbot: started`").
-define(HELPTEXT, "`linusbot: I just abuse people that say 'pull-request' in anyway.`").
-define(QUOTES, ["Christ people. This is just shit.",
                 "Anybody who thinks that code is a) legible, b) efficient or c) safe is just incompetent and out to lunch.",
                 "It looks bad, and there is no reason for it.",
                 "Really. Give me *one* reason why it was written in that idiotic way.",
                 "I really see no reason for this kind of complete idiotic crap.",
                 "I want to make it clear to *everybody* that code like this is completly unacceptable.",
                 "Anybody who thinks code lie that is 'safe' and 'secure' is so far out to lunch that it's not even funny.",
                 "This crap is an unreadable mess that no sane person will ever really understand.",
                 "Get rid of it. And I don't *ever* want to see that shit again.",
                 "Christ, people. Learn C, instead of just stringing random characters together until it compiles (with warnings).",
                 "There are arguments for this kind of code, but they are from weak minds.",
                 "Please people. When I see these kinds of obviously bogus code problems, that just makes me very upset.",
                 "I really am very tired indeed of these 'trivially obvious improvements' that are buggy and actually introduce whole new ways to write buggy code.",
                 "It's clearly total and utter CRAP.",
                 "You've shown yourself to not be competent in this issue, so I'll fix it directly and immediately myself",
                 "I'm angry, because the whole thing is so _horribly_ wrong.",
                 "The whole thing is incredibly broken shit.",
                 "Fix your f*cking code, because it is obviously broken. And fix your approach to programming."]).

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
start_link(ChannelId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ChannelId], []).

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
init([ChannelId]) ->
    syn:join(slack_messages, self()),
    slack_client:send_message(ChannelId,?STARTUPTEXT),
    slack_client:send_message(ChannelId,?HELPTEXT),
    {ok, #state{}}.

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
handle_info([{<<"type">>,<<"message">>},{<<"channel">>,ChannelId},
                 {<<"user">>,_UserId},{<<"text">>,Body},_,_], State) ->
    send_response( string:str(binary_to_list(Body),"pull-request"), ChannelId, pick_quote()),
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

send_response(0, _, _ ) ->
    ok;
send_response(_N, ChannelId, Quote) ->
    slack_client:send_message(ChannelId, Quote).

pick_quote() ->
    [Q|_] = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- ?QUOTES])],
    Q.
