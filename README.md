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

---

Warnings:

* Ineficient key generation;
* Brute force search (gets very slow with larger ring sizes);
* Depends on Erlang R16B03.
* Identifiers are valid for Riak 2 only, haven't implemented the earlier term {bucket/key}.

---

Implementation 

Iterates over a set of partition ranges, for each partition range, attempts to generate a set of keys which confirm to the constraint that we generate a single identifier (type/bucket/key) which hashes into the range, we store the identifier and move to the next range. Intermediate generated keys are discarded as we move to the next partition. 


--- 

Building:

-- 

```
[/common/riak_canary_generator%]./rebar clean get-deps compile
==> neotoma (escriptize)
specify the full pathname of the library, or use the `-LLIBDIR'
flag during linking and do at least one of the following:
   - add LIBDIR to the `DYLD_LIBRARY_PATH' environment variable
     during execution
..... .... etc
Compiled ./src/riak_canary_generator.erl
```

Example use:

```
[/common/riak_canary_generator%]./start
Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]
```

Invocation:

```erlang
2> riak_canary_generator:solve(4, {{<<"type">>,<<"bucket">>},<<"key-">>}).
```

Output:

```erlang
partitions
[{1,{0,365375409332725729550921208179070754913983135743}},
 {2,
  {365375409332725729550921208179070754913983135743,
   730750818665451459101842416358141509827966271486}},
 {3,
  {730750818665451459101842416358141509827966271486,
   1096126227998177188652763624537212264741949407229}},
 {4,{1096126227998177188652763624537212264741949407229,0}}]

 [{{{{<<"type">>,<<"bucket">>},
    <<"key-3140ec02-91ef-11e5-9206-14109fe3abb7">>},
   190529879958580490665822505316274401814640954245},
  {1,{0,365375409332725729550921208179070754913983135743}}},
 {{{{<<"type">>,<<"bucket">>},
    <<"key-31412d84-91ef-11e5-b967-14109fe3abb7">>},
   673137275778639047816088861842134154868037617557},
  {2,
   {365375409332725729550921208179070754913983135743,
    730750818665451459101842416358141509827966271486}}},
 {{{{<<"type">>,<<"bucket">>},
    <<"key-31420f06-91ef-11e5-9699-14109fe3abb7">>},
   868989857133964520153631898425052424737534032535},
  {3,
   {730750818665451459101842416358141509827966271486,
    1096126227998177188652763624537212264741949407229}}},
 {{{{<<"type">>,<<"bucket">>},
    <<"key-31425268-91ef-11e5-9896-14109fe3abb7">>},
   1192984217195104037378983826225943715182541175860},
  {4,{1096126227998177188652763624537212264741949407229,0}}}]
```
