%%%-------------------------------------------------------------------
%%% @author bryanhunt
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Nov 2015 17:37
%%%-------------------------------------------------------------------
-module(riak_canary_generator).
-author("bryanhunt").

%% API
-export([calc/1,randomGen/0]).

-spec randomGen() -> string().
randomGen() ->
  integer_to_list(round(random:uniform() * 10000000000000000000000000000000))
.


-spec calc(integer()) -> list().
calc(RingSize) ->
calc(RingSize, fun riak_canary_generator:randomGen/0)
.


-spec calc(integer(), fun()) -> list().
calc(_,RGFunc)->

RGFunc()

.

