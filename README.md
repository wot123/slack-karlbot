karlbot
=====

A Quick and dirty slack bot written in Erlang

Build
-----

    $ rebar3 compile


Config
-----

Add your bot's oauth token to priv/slack.config (see example file)  
Copy priv/plugins.config_example to priv/plugins.config  
Clone the example plugins repo to priv/plugins  
  
    Eshell V7.1  (abort with ^G)  
    1> application:ensure_all_started(karlbot).  

In Slack
-----

    @botname load linusbot  
    ok  
    @botname start linusbot  
    linusbot: started  

Docker
-----

Build the container
    docker build -t karlbot .

Run the container
    create a directory containing your config files and plugins then:

    docker run -v my_priv_directory:/home/karlbot/lib/karlbot-0.1.0/priv -d -t karlbot
