-module(esnowflake).

-export([
	start/0,
	stop/0,
	
	id/0,
	parse/1,
	parse/2,
	
	test/0,
	test/1
]).

%% 获取id  
%% esnowflake:id().
%% return Id
id()->
	esnowflake_id:id().
%% 解析ID
%% esnowflake:parse(510974385717252097). 
%% return {1545192446750,1,1}
parse(Id)->
	esnowflake_id:parse_id(Id).
parse(Id,Millisecond)->
	esnowflake_id:parse_id(Id,Millisecond).

%% 10w 并发，查看最大seq是否超越8192序
%% esnowflake:test().
test()->
	test(250000).
test(Num)->
	List = lists:seq(1,Num),
	lists:foreach(fun(_)-> 
		spawn(fun()-> 
			id()			  
		end)					  
	end,List),
	esnowflake_id:max_seq().

%% 启动方法
start()->
	%% 含连接从节点过程。
	ok = start(?MODULE),
	ok.

%% 关闭方法
stop()->
	application:stop(?MODULE),
	timer:sleep(5000),
	erlang:halt(),
	ok.	


%% 启动App
start(App) ->
    start_ok(App, application:start(App, permanent)).
start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({aps_start_failed, App, Reason}).







