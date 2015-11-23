%%%-------------------------------------------------------------------
%%% @author  binarytemple
%%% @copyright apache licensed
%%% @doc
%%%
%%% @end
%%% Created : 18. Nov 2015 17:37
%%%-------------------------------------------------------------------
-module(riak_canary_generator).
-author("bryanhunt").

-define(DEBUG, false).

%% API
-export([gen_partitions/1, gen_zipped_partitions/1, solve/2, sha/1, gen_uuid/0]).

-spec gen_partitions(integer()) -> list(integer()).
gen_partitions(RingSize) when is_integer(RingSize) ->
  [((trunc(math:pow(2, 160) - 1) div RingSize - 1) * Res) || Res <- lists:seq(0, RingSize - 1)].

-spec gen_zipped_partitions(integer()) -> list({integer(), {integer(), integer()}}).
gen_zipped_partitions(RingSize) when is_integer(RingSize) ->
  Parts = riak_canary_generator:gen_partitions(RingSize),
  lists:zip(
    lists:map(fun(X) -> X end,
      lists:seq(1, RingSize)
    ), lists:zip(Parts, left(Parts, 1))).


-type bucketType() :: binary().
-type bucket() :: binary().
-type key() :: binary().
-type identifier() :: {{bucketType(), bucket()}, key()}.
-type result() :: [{identifier(), {integer(), integer()}}].

-spec solve(integer(), identifier()) -> result().
solve(RingSize, Identifier) ->
  {{_Type, _Bucket}, _KeyPrefix} = Identifier,
  _ = RingSize,
  Parts = gen_zipped_partitions(RingSize),
  io:format("~n----~npartitions~n~p ~n----~n ", [Parts]),
  solve(Identifier, gen_zipped_partitions(RingSize), []).


solve(Id, [PHead | PTail], Acc) ->
  {_Index, Range} = PHead,
  {{Type, Bucket}, KeyPrefix} = Id,
  GeneratedKey =
    {{Type, Bucket}, <<KeyPrefix/binary, (gen_uuid())/binary>>},
  Sha = sha(GeneratedKey),
  %io:format("trying : ~p:~p - range:~p ~n ", [GeneratedKey, Sha, Range]),
  case Range of
    {Start, End} when End > 0, Sha >= Start, Sha =< End ->
%%-ifdef(debug)
%%io:format("~nFound: ~p~p", [Sha, GeneratedKey]),
%%-endif,
      solve(Id, PTail, Acc ++ [{{GeneratedKey, Sha}, PHead}])
    ;
    {Start, End} when End == 0, Sha >= Start ->
%%-ifdef(debug),
%%io:format("~nFound: ~p~p", [Sha, GeneratedKey]),
%%-endif,
      solve(Id, PTail, Acc ++ [{{GeneratedKey, Sha}, PHead}])
    ;
    _ ->
%%-ifdef(debug),
%%io:format("~nDidn't find ~p~p", [Sha, GeneratedKey]),
%%-endif,
      solve(Id, [PHead | PTail], Acc)
  end
;

solve(_, [], Acc) ->
  Acc.

-spec gen_uuid() -> binary().
gen_uuid() ->
  list_to_binary(uuid:to_string(uuid:uuid1())).

-spec sha(identifier()) -> integer().
sha(Identifier) ->
  crypto:bytes_to_integer(crypto:hash(sha, term_to_binary(Identifier))).

%%
%% Utility functions
%%

left(List, Times) ->
  left(List, Times, []).

left([], Times, Acc) when Times > 0 ->
  left(reverse(Acc), Times, []);
left(List, 0, Acc) ->
  List ++ reverse(Acc);
left([H | T], Times, Acc) ->
  left(T, Times - 1, [H | Acc]).

right(List, Times) ->
  reverse(left(reverse(List), Times)).

reverse(List) ->
  reverse(List, []).

reverse([], Acc) ->
  Acc;
reverse([H | T], Acc) ->
  reverse(T, [H | Acc]).


% - TODO - tests
%
%
%

% This should produce the list of id's and partitions...
% riak_canary_generator:solve({{<<"foo">>,<<"bar">>},<<"baz">>},riak_canary_generator:gen_zipped_partitions(4), []  ).
