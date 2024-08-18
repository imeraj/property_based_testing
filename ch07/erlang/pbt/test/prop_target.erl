-module(prop_target).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

%% Properties
prop_tree_regular(opts) -> [{numtests, 1000}].
prop_tree_regular() ->
  ?FORALL(T, tree(),
    begin
      Weight = sides(T),
      io:format(" ~p", [Weight]),
      true
    end).

prop_tree() ->
  ?FORALL_TARGETED(T, tree(),
    begin
      {Left, Right} = Weight = sides(T),
      io:format(" ~p", [Weight]),
      ?MAXIMIZE(Left-Right),
      true
    end).

prop_tree_neighbour() ->
  ?FORALL_TARGETED(T, ?USERNF(tree(), next_tree()),
    begin
      {Left, Right} = Weight = sides(T),
      io:format(" ~p", [Weight]),
      ?MAXIMIZE(Left-Right),
      true
    end).


%% This one takes long because it does 100 rounds for ?FORALL
%% and 1000 rounds for ?NOT_EXISTS; this gives 100,000 executions!
prop_tree_search() ->
  ?FORALL(L, list(integer()),
    ?NOT_EXISTS(T,
      ?USERNF(
        ?LET(X, L, to_tree(X)),
        next_tree()
      ),
      begin
        {Left, Right} = Weight = sides(T),
        ?MAXIMIZE(Left-Right),
        false
      end)).

%% Generators
next_tree() ->
  fun(OldTree, {_, T}) ->
    ?LET(N, integer(), insert(trunc(N*T*100), OldTree))
  end.

tree() ->
  ?LET(L, non_empty(list(integer())), to_tree(L)).

to_tree(L) ->
  lists:foldl(fun insert/2, undefined, L).

insert(N, {node, N, L, R}) -> {node, N, L, R};
insert(N, {node, M, L, R}) when N < M -> {node, M, insert(N, L), R};
insert(N, {node, M, L, R}) when N > M -> {node, M, L, insert(N, R)};
insert(N, {leaf, N}) -> {leaf, N};
insert(N, {leaf, M}) when N < M -> {node, N, undefined, {leaf, M}};
insert(N, {leaf, M}) when N > M -> {node, N, {leaf, M}, undefined};
insert(N, undefined) -> {leaf, N}.

sides({node, _, Left, Right}) ->
  {LL, LR} = sides(Left),
  {RL, RR} = sides(Right),
  {count_inner(Left) + LL + LR, count_inner(Right) + RL + RR};
sides(_) -> {0,0}.

count_inner({node, _, _, _}) -> 1;
count_inner(_) -> 0.

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