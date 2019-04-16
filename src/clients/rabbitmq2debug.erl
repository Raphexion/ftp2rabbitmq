-module(rabbitmq2debug).
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

client_process(Name, Payload, Topic) ->
    io:fwrite("[~p] accepting ~p bytes on topic ~p~n", [Name, byte_size(Payload), Topic]),
    ok.
