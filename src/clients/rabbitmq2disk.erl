-module(rabbitmq2disk).
-behaviour(gen_f2r_client).

%% API
-export([start_link/4,
	 client_process/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link(Name, Exchange, Queue, RoutingKey) ->
    gen_f2r_client:start_link(?MODULE, Name, Exchange, Queue, RoutingKey).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

client_process(Name, Payload, Key) ->
    lager:debug("writes file to disk ~p ~p ~p ~p", [Name, byte_size(Payload), Key]),
    file:write_file(Name, Payload).
