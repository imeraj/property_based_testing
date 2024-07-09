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

  # Helpers
  def key(), do: integer()
  def val(), do: term()

  # Generators
end
