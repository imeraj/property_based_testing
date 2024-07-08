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

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
model_biggest(List) ->
  lists:last(lists:sort(List)).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
