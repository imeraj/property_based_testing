-module(prop_base).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_last() ->
  ?FORALL({List, KnownLast}, {list(number), number()},
    begin
      KnownList = List ++ [KnownLast],
      KnownLast =:= lists:last(KnownList)
    end).

prop_symmetric() ->
  ?FORALL(Data, list({atom(), any()}),
    begin
      Encoded = encode(Data),
      is_binary(Encoded) andalso Data =:= decode(Encoded)
    end).

prop_dupes() ->
  ?FORALL(KV, list({key(), val()}),
    begin
     M = maps:from_list(KV),
     [maps:get(K, M) || {K, _V} <- KV],
     true
    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
encode(T) -> term_to_binary(T).
decode(T) -> binary_to_term(T).

key() -> integer().
val() -> term().

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
