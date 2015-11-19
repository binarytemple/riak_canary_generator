# Riak canary object generator


Canary objects - detect problems with replication	

This is useful for testing full-sync and real-time Riak MDC replication. 

The explanatory rationale is detailed in  [this](http://docs.basho.com/riakee/latest/cookbooks/Multi-Data-Center-Replication-Monitoring/) Basho Document.
 
Our intention is to:
* Generate one object per Riak logical partition given any valid ring size e.g 8,16,32,64,2048.
* Write the object to the source cluster.
* Periodically poll the sink cluster, giving this covering set of objects.
* Read the object from the sink cluster, thus verifying that replication is working correctly for the given VNodes.
* 

--

The implementation of this code will be to generate a load of unique object identifiers (type/bucket/key)/(bucket/key) chash them in turn until we extract a set of object identifiers where we have one object identifier per partition.
