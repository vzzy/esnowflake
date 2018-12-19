%%%-------------------------------------------------------------------
%% @doc esnowflake top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(esnowflake_sup).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	Millisecond = case application:get_env(millisecond) of
		{ok, M} ->
			M;
		_->
			esnowflake_id:get_default_TIMESTAMP()
	end,
    {ok, { {one_for_one,1000, 600}, [{esnowflake_id,{esnowflake_id,start_link,[Millisecond]},permanent,2000,worker,[esnowflake_id]}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
