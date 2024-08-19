defmodule Cache do
  use GenServer

  # API
  def start_link(n) do
    GenServer.start_link(__MODULE__, n, name: __MODULE__)
  end

  def stop() do
    GenServer.stop(__MODULE__)
  end

  def find(key) do
    case :ets.match(:cache, {:_, {key, :"$1"}}) do
      [[val]] -> {:ok, val}
      [] -> {:error, :not_found}
    end
  end

  def flush(), do: GenServer.call(__MODULE__, :flush)

  def cache(key, val), do: GenServer.call(__MODULE__, {:cache, key, val})

  # Callbacks
  def init(n) do
    :ets.new(:cache, [:public, :named_table])
    :ets.insert(:cache, {:count, 0, n})

    {:ok, :nostate}
  end

  def handle_call({:cache, key, val}, _from, state) do
    case :ets.match(:cache, {:"$1", {key, :_}}) do
      [[n]] ->
        :ets.insert(:cache, {n, {key, val}})

      [] ->
        :erlang.yield()

        case :ets.lookup(:cache, :count) do
          [{:count, max, max}] ->
            :ets.insert(:cache, [{1, {key, val}}, {:count, 1, max}])

          [{:count, current, max}] ->
            :ets.insert(:cache, [
              {current + 1, {key, val}},
              {:count, current + 1, max}
            ])
        end
    end

    {:reply, :ok, state}
  end

  def handle_call(:flush, _from, state) do
    [{:count, _, max}] = :ets.lookup(:cache, :count)
    :ets.delete_all_objects(:cache)
    :erlang.yield()
    :ets.insert(:cache, {:count, 0, max})
    {:reply, :ok, state}
  end

  def handle_cast(_cast, state), do: {:noreply, state}
  def handle_info(_msg, state), do: {:noreply, state}
end
