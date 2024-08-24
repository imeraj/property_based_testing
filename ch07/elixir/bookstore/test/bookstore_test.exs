defmodule BookstoreTest do
  use ExUnit.Case
  use PropCheck
  use PropCheck.StateM
  doctest Bookstore

  property "bookstore stateful operations", [:verbose] do
    forall cmds <- commands(__MODULE__) do
      # No setup macro in PropCheck yet, do it all inline
      {:ok, apps} = Application.ensure_all_started(:bookstore)
      Bookstore.DB.setup()
      {history, state, result} = run_commands(__MODULE__, cmds)
      Bookstore.DB.teardown()
      for app <- apps, do: Application.stop(app)

      (result == :ok)
      |> aggregate(command_names(cmds))
      |> when_fail("""
      IO.puts(\"""
      History: #{inspect(history)}
      State: #{inspect(state)}
      Result: #{inspect(result)}
      """)
    end
  end

  def initial_state, do: %{}

  def command(_state) do
    oneof([
      {:call, Bookstore.DB, :add_book, [isbn(), title(), author(), 1, 1]},
      {:call, Bookstore.DB, :add_copy, [isbn()]},
      {:call, Bookstore.DB, :borrow_copy, [isbn()]},
      {:call, Bookstore.DB, :return_copy, [isbn()]},
      {:call, Bookstore.DB, :find_book_by_author, [author()]},
      {:call, Bookstore.DB, :find_book_by_title, [title()]},
      {:call, Bookstore.DB, :find_book_by_isbn, [isbn()]}
    ])
  end

  def precondition(_state, {:call, _mod, _fun, _args}) do
    true
  end

  def postcondition(_state, {:call, _mod, _fun, _args}, _res) do
    true
  end

  def next_state(state, _res, {:call, _mod, _fun, _args}) do
    new_state = state
    new_state
  end

  def title do
    let s <- utf8() do
      elements([s, String.to_charlist(s)])
    end
  end

  def author do
    let s <- utf8() do
      elements([s, String.to_charlist(s)])
    end
  end

  def isbn() do
    let isbn <- [
          oneof([~c"978", ~c"979"]),
          let(x <- range(0, 9999), do: to_charlist(x)),
          let(x <- range(0, 9999), do: to_charlist(x)),
          let(x <- range(0, 999), do: to_charlist(x)),
          frequency([{10, [range(?0, ?9)]}, {1, ~c"X"}])
        ] do
      to_string(Enum.join(isbn, "-"))
    end
  end
end
