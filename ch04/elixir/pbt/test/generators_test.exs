defmodule GeneratorsTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "find all keys in a map even when dupes are used" do
    forall kv <- list({key(), val()}) do
      m = Map.new(kv)
      for {k, _v} <- kv, do: Map.fetch!(m, k)
      true
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
  def key(), do: integer()
  def val(), do: term()

  def to_range(m, n) do
    base = div(n, m)
    {base * m, (base + 1) * m}
  end
  # Generators
end
