-module(luerl_lib_karl).

-compile([{parse_transform, lager_transform}]).

-export([install/1]).

install(St) ->
    luerl_emul:alloc_table(table(), St).


table() ->
    [{<<"gun_test">>,{function, fun gun_test/2}},
     {<<"send_message">>,{function, fun send_message/2}},
     {<<"http_get">>,{function, fun http_get/2}},
     {<<"db_store">>,{function, fun db_store/2}},
     {<<"db_get">>,{function, fun db_get/2}},
     {<<"register_message_handler">>,{function, fun register_handler/2}}].

gun_test(Args, State) ->
    lager:info("gun_test called: ~p",[Args]),
    {[ok], State}.


send_message([Channel, Message], State) ->
    lager:info("lua send message: ~p ~p",[Channel, Message]), 
    ok = slack_client:send_message(Channel, binary_to_list(Message)),
    {[ok], State}.

http_get(Args, State) ->
    lager:info("lua http_get: ~p",[Args]),
    {[ok], State}.

db_store(Args, State) ->
    lager:info("lua db_store: ~p",[Args]),
    {[ok], State}.

db_get(Args, State) ->
    lager:info("lua db_get: ~p", [Args]),
    {[ok], State}.

register_handler([Fun], State) ->
    lager:info("register handler ~p",[Fun]),
    {[ok], State}.


