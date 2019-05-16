FROM erlang AS builder

WORKDIR /app

ADD . .

RUN rebar3 release
RUN rebar3 tar

FROM erlang

ENV rabbitmq_username=guest
ENV rabbitmq_password=guest
ENV rabbitmq_hostname=localhost
ENV rabbitmq_port=5672

WORKDIR /app

COPY --from=builder /app/_build/default/rel/ftp2rabbitmq/ftp2rabbitmq-0.2.0.tar.gz .
RUN tar xvf ./ftp2rabbitmq-0.2.0.tar.gz -C /usr/local

EXPOSE 2121

CMD ["/usr/local/bin/ftp2rabbitmq", "foreground"]
