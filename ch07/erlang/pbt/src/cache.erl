-module(cache).
-export([start_link/1, stop/0, cache/2, find/1, flush/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-behaviour(gen_server).

%% API
start_link(N) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, N, []).

find(Key) ->
  case ets:match(cache, {'_', {Key, '$1'}}) of
    [[Val]] -> {ok, Val};
    [] -> {error, not_found}
  end.

cache(Key, Val) ->
  case ets:match(cache, {'$1', {Key, '_'}}) of
    [[N]] ->
      ets:insert(cache, {N, {Key, Val}});

    [] ->
      case ets:lookup(cache, count) of
        [{count, Max, Max}] ->
          ets:insert(cache, [{1, {Key, Val}}, {count, 0, Max}]);
        [{count, Current, Max}] ->
          ets:insert(cache, [{Current+1, {Key, Val}}, {count, Current+1, Max}])
      end
  end.

flush() ->
  [{count,_,Max}] = ets:lookup(cache, count),
  ets:delete_all_objects(cache),
  ets:insert(cache, {count, 0, Max}).

stop() ->
  gen_server:stop(?MODULE).

%% Callbacks
init(N) ->
  ets:new(cache, [public, named_table]),
  ets:insert(cache, {count, 0, N}),
  {ok, nostate}.

handle_call(_Call, _From, State) ->
  {noreply, State, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.
