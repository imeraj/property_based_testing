defmodule PbtTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "find biggest element" do
    forall list <- non_empty(list(integer())) do
      Pbt.biggest(list) == model_biggest(list)
    end
  end

  property "picks the last number" do
    forall {list, known_last} <- {list(number()), number()} do
      known_list = list ++ [known_last]
      known_last == List.last(known_list)
    end
  end

  property "a sorted list has ordered pairs" do
    forall list <- list(term()) do
      is_ordered(Enum.sort(list))
    end
  end

  property "a sorted list keeps its size" do
    forall list <- list(number()) do
      length(list) == length(Enum.sort(list))
    end
  end

  property "no element added in a sorted list" do
    forall list <- list(number()) do
      sorted_list = Enum.sort(list)
      Enum.all?(sorted_list, & &1 in list)
    end
  end

  property "no element deleted from original list" do
    forall list <- list(number()) do
      sorted_list = Enum.sort(list)
      Enum.all?(list, & &1 in sorted_list)
    end
  end

  property "symmetric encoding/decoding" do
    forall data <- list({atom(), any()}) do
        encoded = encode(data)
       is_binary(encoded) and data == decode(encoded)
    end
  end


  # Helpers
  def model_biggest(list), do: List.last(Enum.sort(list))

  def encode(t), do: :erlang.term_to_binary(t)
    def decode(t), do: :erlang.binary_to_term(t)

    def is_ordered([a, b | t]) do
    a <= b and is_ordered([b | t])
  end

  # lists with fewer than 2 elements
  def is_ordered(_), do: true

  # Generators
end
