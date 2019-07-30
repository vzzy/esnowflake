%% @author bai
%% @doc @todo Add description to esnowflake.


-module(esnowflake_id).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% since 2019-01-01 00:00:00.000
-define(TIMESTAMP, 1546272000000). 
%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start_link/1,

	id/0,
	set_node_id/1,
	parse_id/1,
	parse_id/2,
	
	get_node_id/0,
	get_default_TIMESTAMP/0
]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
	node_id = none,
	timestamp = ?TIMESTAMP,
	
	now = 0,
	seq = 0,

	max_seq = 0
}).

%% 启动 {ok,Pid} | ignore | {error,Error}
start_link(Millisecond) ->
    gen_server:start_link({local,?MODULE},?MODULE, [Millisecond], []).

id()->
	gen_server:call(?MODULE,id).

get_node_id()->
	gen_server:call(?MODULE,get_node_id).

set_node_id(Node_id) when Node_id>=0 andalso Node_id<4096->
	gen_server:call(?MODULE,{set_node_id,Node_id});
set_node_id(_Node_id)->
	{error,<<"node id must range of [0,4096)">>}.

max_seq()->
	gen_server:call(?MODULE,max_seq).

init([Millisecond]) ->
    {ok, #state{
		timestamp = Millisecond,
		seq = 0	
	}}.

handle_call(get_node_id, _From, #state{node_id=Node_id} = State) ->
	{reply, {ok,Node_id}, State};
handle_call({set_node_id,Node_id}, _From, State) when Node_id>=0 andalso Node_id<4096->
	{reply, {ok,node()}, State#state{node_id=Node_id}};
handle_call(id, _From, #state{node_id = none} = State) ->
	{reply, {error,<<"node id must range of [0,4096)">>}, State};
handle_call(id, _From, #state{
			node_id = Node_id,					
			timestamp = Millisecond,
			
			now = L_Now,
			seq = Seq,
			max_seq = Max_seq
		} = State) ->
	Now = get_timestamp(),
	New_Seq = if
		Now /= L_Now -> %% 时间不一样
			1;
		true->
			Seq + 1
	end,
	New_Max_seq = if
		New_Seq>Max_seq ->
			New_Seq;
		true->
			Max_seq
	end,
	<<Id:64>> = make_id(Now,Millisecond,Node_id,New_Seq),
    {reply, {ok,Id}, State#state{
		now = Now,
		seq = New_Seq,
		max_seq = New_Max_seq
	}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 获取ID
make_id(Now,Millisecond,Node_id,Seq)->
	Gap = Now - Millisecond,
	<<Gap:40,Node_id:12,Seq:12>>.

%% 反解ID
parse_id(Id)->
	parse_id(Id,?TIMESTAMP).
parse_id(Id,Millisecond) when is_integer(Id)->
	parse_id(<<Id:64>>,Millisecond);
parse_id(Id,Millisecond)->
	<<Gap:40,Node_id:12,Seq:12>> = Id,
	{Gap + Millisecond,Node_id,Seq}.

%%获取从公元1970年到当前时间的毫秒数
get_timestamp() ->
	{MegaSecs,Secs,Micro} = os:timestamp(),
	(MegaSecs*1000000 + Secs)*1000 + Micro div 1000.

get_default_TIMESTAMP()->
	?TIMESTAMP.

