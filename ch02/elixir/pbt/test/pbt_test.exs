defmodule PbtTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "find biggest element" do
    forall list <- non_empty(list(integer())) do
      biggest(list) == List.last(Enum.sort(list))
    end
  end

  # Helpers
  def biggest([head | tail]) do
    biggest(tail, head)
  end

  defp biggest([], max) do
    max
  end

  defp biggest([head | tail], max) when head >= max do
    biggest(tail, head)
  end

  defp biggest([head | tail], max) when head < max do
    biggest(tail, max)
  end

  # Generators
end
