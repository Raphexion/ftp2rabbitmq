ftp2rabbitmq
============

Under development. Very unstable.

Design
------

![Big Picture](doc/big_picture.png)

Gettings started
----------------

Start RabbitMQ

```
docker run -d --rm -p 5672:5672 -p 15672:15672 rabbitmq:3.7-management
```

Start application

```
rebar3 shell
```

For development purposes you can create a client that listens to the queue.
Please note that `foobar` must be the same as the username when logging into
the ftp server.

```
rabbitmq2debug:start_link("data", "ftpdata", "ftp_data1", "#").
rabbitmq2debug:start_link("info", "ftpinfo", "ftp_info1", "#").
```

Configure RabbitMQ broker
-------------------------

```
export rabbitmq_username="guest"
export rabbitmq_password="guest"
export rabbitmq_hostname="localhost"
export rabbitmq_port="5672"
```
