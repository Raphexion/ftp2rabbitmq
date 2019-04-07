%%%-------------------------------------------------------------------
%% @doc ftp2rabbitmq top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(file2rabbitmq_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_file2rabbitmq/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_file2rabbitmq(Fp, Mq) ->
    supervisor:start_child(?MODULE, [Fp, Mq]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    File2RabbitMq = #{id => file2rabbitmq,
		      start => {file2rabbitmq, start_link, []},
		      restart => transient},
    Children = [File2RabbitMq],

    {ok, {{simple_one_for_one, 1, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
