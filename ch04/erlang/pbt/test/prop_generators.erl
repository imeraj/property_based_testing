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
     true
    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
key() -> integer().
val() -> term().

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
