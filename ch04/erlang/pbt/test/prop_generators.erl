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

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
key() -> oneof([range(1, 10), integer()]).
val() -> term().

to_range(M, N) ->
  Base = N div M,
  {Base*M, (Base+1)*M}.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
