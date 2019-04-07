%%%-------------------------------------------------------------------
%% @doc ftp2rabbitmq top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ftp2rabbitmq_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    FileProducer = #{id => file_producer,
		     start => {file_producer, start_link, []}},

    Bifrost = #{id => bifrost,
		start => {bifrost, start_link, [ftp2rabbitmq,
						[{port, 2121}]]}},

    File2RabbitMqSup = #{id => file2rabbitmq_sup,
			 start => {file2rabbitmq_sup, start_link, []}},

    Children = [FileProducer,
		Bifrost,
		File2RabbitMqSup],

    {ok, {{one_for_all, 1, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
