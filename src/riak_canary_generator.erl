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
-export([calc/1, randomGen/0, gen_partitions/1, left/2, right/2]).

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

-spec gen_partitions(integer()) -> list(integer()).
gen_partitions(RingSize) when is_integer(RingSize) ->
[  ((trunc(math:pow(2,160)-1) div RingSize) * Res  )|| Res <- lists:seq(0,RingSize)].


left(List, Times) ->
  left(List, Times, []).

left([], Times, Acc) when Times > 0 ->
  left(reverse(Acc), Times, []);
left(List, 0, Acc) ->
  List ++ reverse(Acc);
left([H|T], Times, Acc) ->
  left(T, Times-1, [H|Acc]).

right(List, Times) ->
  reverse(riak_canary_generator:left(reverse(List), Times)).

reverse(List) ->
  reverse(List, []).

reverse([], Acc) ->
  Acc;
reverse([H|T], Acc) ->
  reverse(T, [H|Acc]).
