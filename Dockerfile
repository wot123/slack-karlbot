FROM erlang:19
RUN groupadd -g 10000 karlbot
RUN useradd -c "Karlbot" -d /home/karlbot -u 10000 -g 10000 -m karlbot
USER karlbot
WORKDIR /home/karlbot
RUN git clone https://github.com/wot123/slack-karlbot && \
    cd slack-karlbot && \
    rebar3 release tar && \
    cd ~ && \
    tar -xvf slack-karlbot/_build/default/rel/karlbot/karlbot-0.1.0.tar.gz && \
    rm -rf slack-karlbot
VOLUME "/home/karlbot/lib/karlbot-0.1.0/priv"
ENTRYPOINT ["bin/karlbot", "console"]
