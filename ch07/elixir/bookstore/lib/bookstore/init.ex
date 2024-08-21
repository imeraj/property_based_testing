defmodule Bookstore.Init do
  @moduledoc false

  def main(_) do
    stdout = IO.stream(:stdio, :line)

    IO.puts("setting up `bookstore_db` database")
    System.cmd(
      "psql",
      ["-c", "CREATE DATABASE bookstore_db;",
       "-h", "localhost",
       "-d", "template1"
      ],
      into: stdout
    )
    IO.puts("OK.")
    :init.stop()
  end
end