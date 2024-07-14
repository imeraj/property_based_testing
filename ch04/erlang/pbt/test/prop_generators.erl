-module(prop_generators).
-include_lib("proper/include/proper.hrl").

-export([even/0, odd/0, text_like/0, mostly_sorted/0, path/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_dupes() ->
  ?FORALL(KV, list({key(), val()}),
    begin
     M = maps:from_list(KV),
     [maps:get(K, M) || {K, _V} <- KV],
     collect({dupes, to_range(5, length(KV) - length(lists:ukeysort(1, KV)))}, true)
    end).

prop_collect1() ->
  ?FORALL(Bin, binary(), collect(byte_size(Bin), is_binary(Bin))).

prop_collect2() -> ?FORALL(Bin, resize(150, binary()),
  collect(to_range(10, byte_size(Bin)), is_binary(Bin))).

prop_aggregate() ->
  Suits = [club, diamond, heart, spade],
  ?FORALL(Hand, vector(5, {oneof(Suits), choose(1, 13)}), aggregate(Hand, true)).

prop_escape() ->
  ?FORALL(Str, string(), aggregate(classes(Str), escape(Str))).

prop_profile1() ->
  ?FORALL(Profile, [{name, resize(10, string())},
                    {age, pos_integer()},
                    {bio, resize(350, string())}],
    begin
      NameLen = to_range(10, length(proplists:get_value(name, Profile))),
      BioLen = to_range(10, length(proplists:get_value(bio, Profile))),
      aggregate([{name, NameLen}, {bio, BioLen}], true)
    end).

prop_profile2() ->
  ?FORALL(Profile, [{name, string()},
    {age, pos_integer()},
    {bio, ?SIZED(Size, resize(Size * 35, string()))}],
    begin
      NameLen = to_range(10, length(proplists:get_value(name, Profile))),
      BioLen = to_range(10, length(proplists:get_value(bio, Profile))),
      aggregate([{name, NameLen}, {bio, BioLen}], true)
    end).

prop_queue_naive() ->
  ?FORALL(List, list({term(), term()}),
    begin
      Queue = queue:from_list(List),
      queue:is_queue(Queue)
    end).

prop_queue_nicer() ->
  ?FORALL(Queue, queue(),
    begin
      queue:is_queue(Queue)
    end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
key() -> oneof([range(1, 10), integer()]).
val() -> term().

escape(_) -> true.

classes(Str) ->
  L = letters(Str),
  N = numbers(Str),
  P = punctuation(Str),
  O = length(Str) - (L+N+P),
  [{letters, to_range(5, L)},{numbers, to_range(5, N)},
    {punctuation, to_range(5, P)},{others, to_range(5, O)}].

letters(Str) ->
  length([1 || Char <- Str,
    (Char >= $A andalso Char =< $Z) orelse
    (Char >= $a andalso Char =< $z)]).

numbers(Str) ->
  length([1 || Char <- Str, Char >= $0 andalso Char =< $9]).

punctuation(Str) ->
  length([1 || Char <- Str, lists:member(Char, ".,;:'\"-")]).

to_range(M, N) ->
  Base = N div M,
  {Base*M, (Base+1)*M}.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
queue() ->
  ?LET(List, list({term(), term()}),
      queue:from_list(List)).

even() ->
  ?SUCHTHAT(N, integer(), N rem 2 =:= 0).

odd() ->
  ?SUCHTHAT(N, integer(), N rem 2 =/= 0).

text_like() ->
  list(frequency([
    {80, range($a, $z)},
    {10, $\s},
    {1, $\n},
    {1, oneof([$., $-, $!, $?, $,])},
    {1, range($0, $9)}
  ])).

mostly_sorted() ->
  ?LET(Lists,
    list(frequency([
      {5, sorted_list()},
      {1, list(integer())}
    ])),
    lists:append(Lists)).

sorted_list() ->
  ?LET(List, list(integer()), lists:sort(List)).

path() ->
  ?SIZED(Size, path(Size, {0,0}, [], #{{0,0} => seen}, [])).

path(0, _Current, Acc, _Seen, _Ignore) ->
  Acc;
path(_Max, _Current, Acc, _Seen, [_,_,_,_]) ->
  Acc;
path(Max, Current, Acc, Seen, Ignore) ->
  increase_path(Max, Current, Acc, Seen, Ignore).

increase_path(Max, Current, Acc, Seen, Ignore) ->
  DirectionGen = oneof([left, right, up, down] -- Ignore),
  ?LET(Direction, DirectionGen,
    begin
      NewPos = move(Direction, Current),
      case Seen of
        #{NewPos := _} ->
          path(Max, Current, Acc, Seen, [Direction|Ignore]);
        _ ->
          path(Max-1, NewPos, [Direction|Acc], Seen#{NewPos => seen}, [])
      end
    end).

move(left, {X,Y}) -> {X-1, Y};
move(right, {X,Y}) -> {X+1,Y};
move(up, {X,Y}) -> {X,Y+1};
move(down, {X,Y}) -> {X,Y-1}.

