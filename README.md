esnowflake
=====

An Id generator base on Twitter's snowflake-64bit in Erlang.

	结构：一个Long类型数字。 
		64bit = 40bit(34年) + 12bit(4096机) + 12bit(4095序) 
		  可用34年，4096台机器集群，每毫秒4095个序列。
	
	优点： 唯一升序数字、分布式、高可用、含时间。
	缺点： 强依赖时钟，回拨机器时钟，会导致重复。 配合DB唯一性约束，基本能回避这个问题。
	
	起始时间可配置，默认 2018-01-01 00:00:00.000
	{esnowflake,[
		{millisecond,1514736000000}
	]}

Build
-----

    $ make all
    > esnowflake:id().
    510974385717252097
    > esnowflake:parse(510974385717252097).
    {1545192446750,1,1}
    
