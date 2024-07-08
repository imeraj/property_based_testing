-module(prop_thinking).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_biggest() ->
  ?FORALL(List, non_empty(list(integer())),
    begin
      thinking:biggest(List) =:= model_biggest(List)
    end).

prop_sort() ->
  ?FORALL(List, list(term()), is_ordered(lists:sort(List))).

prop_same_size() ->
  ?FORALL(List, list(number()),
    begin
      length(List) =:= length(lists:sort(List))
    end).

prop_no_added() ->
  ?FORALL(List, list(number()),
    begin
      Sorted = lists:sort(List),
      lists:all(fun(Element) -> lists:member(Element, List) end, Sorted)
    end).

prop_no_removed() ->
  ?FORALL(List, list(number()),
    begin
      Sorted = lists:sort(List),
      lists:all(fun(Element) -> lists:member(Element, Sorted) end, List)
    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
model_biggest(List) ->
  lists:last(lists:sort(List)).

is_ordered([A,B|T]) ->
  A =< B andalso is_ordered([B|T]);
is_ordered(_) ->
  true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
