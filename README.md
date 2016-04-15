karlbot
=====

A Quick and dirty slack bot written in Erlang

Build
-----

    $ rebar3 compile


Config
-----

Add your bot's oauth token to priv/slack.config (see example file)

In Slack
-----

@{botname} load linusbot
ok
@{botname} start linusbot
linusbot: started

.... don't say "pull-request"

