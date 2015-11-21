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
-export([calc/1, randomGen/0, gen_partitions/1, left/2, right/2, gen_zipped_partitions/1, solve/2, sha/1, gen_uuid/0, recurso/3]).

-spec randomGen() -> string().
randomGen() ->
  integer_to_list(round(random:uniform() * 10000000000000000000000000000000))
.

-spec calc(integer()) -> list().
calc(RingSize) ->
  calc(RingSize, fun riak_canary_generator:randomGen/0)
.

-spec calc(integer(), fun()) -> list().
calc(_, RGFunc) ->
  RGFunc()
.

-spec gen_partitions(integer()) -> list(integer()).
gen_partitions(RingSize) when is_integer(RingSize) ->
  [((trunc(math:pow(2, 160) - 1) div RingSize - 1) * Res) || Res <- lists:seq(0, RingSize - 1)].

-spec gen_zipped_partitions(integer()) -> list({integer(), {integer(), integer()}}).
gen_zipped_partitions(RingSize) when is_integer(RingSize) ->
  Parts = riak_canary_generator:gen_partitions(RingSize),
  lists:zip(
    lists:map(fun(X) -> X end,
      lists:seq(1, RingSize)
    ), lists:zip(Parts, riak_canary_generator:left(Parts, 1))).


% -
% @edoc ring size -> [ring size {type,bucker,keyPrefix} -> [identifier, partition id,partition startindex]
% each of the identifiers will be unique
%

-type bucketType() :: binary().
-type bucket() :: binary().
-type key() :: binary().
-type identifier() :: {{bucketType(), bucket()}, key()}.
-type result() :: [{identifier(), {integer(), integer()}}].

% -

-spec solve(integer(), identifier()) -> result().
solve(RingSize, Identifier) ->
  {{_Type, _Bucket}, _KeyPrefix} = Identifier,
  _ = RingSize,
  Parts = gen_zipped_partitions(RingSize),
  io:format("~n----~npartitions~n~p ~n----~n ", [Parts]),
  riak_canary_generator:recurso(Identifier, riak_canary_generator:gen_zipped_partitions(RingSize), []).


recurso(Id, [PHead | PTail], Acc) ->
  {_Index, Range} = PHead,
  {{Type, Bucket}, KeyPrefix} = Id,
  GeneratedKey =
    {{Type, Bucket}, <<KeyPrefix/binary, (gen_uuid())/binary>>},
  Sha = sha(GeneratedKey),
  %io:format("trying : ~p:~p - range:~p ~n ", [GeneratedKey, Sha, Range]),
  case Range of
    {Start, End} when End > 0, Sha >= Start, Sha =< End ->
      %io:format("Found: ~p~p", [Sha, GeneratedKey]),
      recurso(Id, PTail, Acc ++ [{{GeneratedKey, Sha}, PHead}])
    ;
    {Start, End} when End == 0, Sha >= Start ->
      %io:format("Found: ~p~p", [Sha, GeneratedKey]),
      recurso(Id, PTail, Acc ++ [{{GeneratedKey, Sha}, PHead}])
    ;
    _ ->
      recurso(Id, [PHead | PTail], Acc)
  end
;


recurso({{_, _}, _}, [], Acc) ->
  Acc.





-spec gen_uuid() -> binary().
gen_uuid() ->
  list_to_binary(uuid:to_string(uuid:uuid1())).

-spec sha(identifier()) -> integer().
sha(Identifier) ->
  crypto:bytes_to_integer(crypto:hash(sha, term_to_binary(Identifier))).


%%
%%-spec solve(integer(),{binary(), binary(),binary()}, Input, Acc )  -> result().
%%solve(RingSize,Prefix,Acc,[]) ->
%%  [].
%%%%x( Candidates, Prefix,  )
%%
%%%%locate_partition( ) ->
%%%%  1.


%% - probably not required - util functions for shift/rotate elements of a list
%% - was going to use with lists:zip/2 but simpler to just increment and stop when we find
%% that our candidate is larger than the previous but smaller than the next element.

left(List, Times) ->
  left(List, Times, []).

left([], Times, Acc) when Times > 0 ->
  left(reverse(Acc), Times, []);
left(List, 0, Acc) ->
  List ++ reverse(Acc);
left([H | T], Times, Acc) ->
  left(T, Times - 1, [H | Acc]).

right(List, Times) ->
  reverse(riak_canary_generator:left(reverse(List), Times)).

reverse(List) ->
  reverse(List, []).

reverse([], Acc) ->
  Acc;
reverse([H | T], Acc) ->
  reverse(T, [H | Acc]).


% - Tests time !
%
%
%

% This should produce the list of id's and partitions...
% riak_canary_generator:recurso({{<<"foo">>,<<"bar">>},<<"baz">>},riak_canary_generator:gen_zipped_partitions(4), []  ).