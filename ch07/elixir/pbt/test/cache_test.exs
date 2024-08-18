defmodule CacheTest do
  use ExUnit.Case
  use PropCheck
  use PropCheck.StateM

  @cache_size 10

  defmodule State do
    @cache_size 10
    defstruct max: @cache_size, count: 0, entries: []
  end

  property "stateful property", [:verbose] do
    forall cmds <- commands(__MODULE__) do
      Cache.start_link(@cache_size)
      {history, state, result} = run_commands(__MODULE__, cmds)
      Cache.stop()

      (result == :ok)
      |> aggregate(command_names(cmds))
      |> when_fail(
        IO.puts("""
            History: #{inspect(history)}
            State: #{inspect(state)}
            Result: #{inspect(result)}
        """)
      )
    end
  end

  def initial_state, do: %State{}

  def command(_state) do
    frequency([
      {1, {:call, Cache, :find, [key()]}},
      {3, {:call, Cache, :cache, [key(), val()]}},
      {1, {:call, Cache, :flush, []}}
    ])
  end

  def precondition(%State{count: 0}, {:call, Cache, :flush, []}), do: false
  def precondition(%State{}, {:call, _Mod, _fun, _args}), do: true

  def postcondition(%State{}, {:call, _Mod, _fun, _args}, _res), do: true

  def next_state(state, _res, {:call, Cache, :flush, _}),
    do: %State{state | count: 0, entries: []}

  def next_state(state, _res, {:call, Cache, :cache, [k, v]}) do
    %{count: n, max: m, entries: l} = state

    case List.keyfind(l, k, 0) do
      nil when n == m ->
        %{state | entries: tl(l) ++ [{k, v}]}

      nil when n < m ->
        %{state | count: n + 1, entries: l ++ [{k, v}]}

      {^k, _} ->
        %{state | entries: List.keyreplace(l, k, 0, {k, v})}
    end
  end

  def next_state(state, _res, {:call, Cache, :find, [k]}), do: state

  def key, do: oneof([range(1, @cache_size), integer()])

  def val, do: integer()
end
