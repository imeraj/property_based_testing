defmodule GeneratorsTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "find all keys in a map even when dupes are used",  [:verbose] do
    forall kv <- list({key(), val()}) do
      m = Map.new(kv)
      for {k, _v} <- kv, do: Map.fetch!(m, k)

      uniques =
        kv
        |> List.keysort(0)
        |> Enum.dedup_by(&elem(&1, 0))

      collect(true, {:dupes, to_range(5, length(kv) - length(uniques))})
    end
  end

  property "collect 1", [:verbose] do
    forall bin <- binary() do
      #    test                   metric
      collect(is_binary(bin), byte_size(bin))
    end
  end

  property "collect 2", [:verbose] do
    forall bin <- binary() do
      #    test                   metric
      collect(is_binary(bin), to_range(10, byte_size(bin)))
    end
  end

  # Helpers
  def key(), do: oneof([range(1,10), integer()])
  def val(), do: term()

  def to_range(m, n) do
    base = div(n, m)
    {base * m, (base + 1) * m}
  end

  # Generators
end
