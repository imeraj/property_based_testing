%%%-------------------------------------------------------------------
%% @doc bookstore top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bookstore_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    bookstore_db:load_queries(),
    {ok, {{one_for_all, 0, 1}, []}}.

