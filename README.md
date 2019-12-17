esnowflake
=====

An Id generator base on Twitter's snowflake-64bit in Erlang.

	结构：一个Long类型数字。 
		64bit = 40bit(69年) + 12bit(4096机) + 12bit(4096序 2^12) 
		  可用69年，4096台机器集群，每毫秒4095个序列。
	
	优点： 唯一升序数字、分布式、高可用、含时间。
	缺点： 强依赖时钟，回拨机器时钟，会导致重复。 配合DB唯一性约束，基本能回避这个问题。
	
	起始时间可配置，默认 2019-01-01 00:00:00.000
	{esnowflake,[
		{millisecond,1546272000000}
	]}

Build
-----

    $ make all
    > {ok,Node} = esnowflake:set_node_id(1).
    > {ok,Id} = esnowflake:id().
    {ok,<<"289310607675393">>}
    > esnowflake:parse(binary_to_integer(Id)).
    {1564480497625,1,1}
    
    > {ok,Node_id} = esnowflake:get_node_id().
    
    > esnowflake:test(250000).
    1.691636
    
    
