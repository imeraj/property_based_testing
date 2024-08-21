-module(bookstore_init).
-export([main/1]).

main(_) ->
  io:format("setting up 'bookstore_db' database...~n"),
  cmd("psql -h localhost -d template1 -c "
    "\"CREATE DATABASE bookstore_db;\""),
  io:format("OK.~n"),
  init:stop().

cmd(Command) -> io:format("~s~n", [os:cmd(Command)]).