-module(prop_target).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

prop_path(opts) -> [{search_steps, 100}].
prop_path() ->
  ?FORALL_TARGETED(P, path(),
      begin
        {X,Y} = lists:foldl(fun move/2, {0,0}, P),
        io:format("~p", [{X,Y}]),
        ?MAXIMIZE(X-Y),
        true
      end).

path() -> list(oneof([left, right, up, down])).

move(left, {X,Y}) -> {X-1,Y};
move(right, {X,Y}) -> {X+1,Y};
move(up, {X,Y}) -> {X,Y+1};
move(down, {X,Y}) -> {X,Y-1}.