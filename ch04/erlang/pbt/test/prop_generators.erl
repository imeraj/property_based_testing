-module(prop_generators).
-include_lib("proper/include/proper.hrl").

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

prop_collect2() -> ?FORALL(Bin, binary(),
  collect(to_range(10, byte_size(Bin)), is_binary(Bin))).

prop_aggregate() ->
  Suits = [club, diamond, heart, spade],
  ?FORALL(Hand, vector(5, {oneof(Suits), choose(1, 13)}), aggregate(Hand, true)).

prop_escape() ->
  ?FORALL(Str, string(), aggregate(classes(Str), escape(Str))).

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
