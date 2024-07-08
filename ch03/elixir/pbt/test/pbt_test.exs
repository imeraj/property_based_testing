defmodule PbtTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "find biggest element" do
    forall list <- non_empty(list(integer())) do
      Pbt.biggest(list) == model_biggest(list)
    end
  end

  # Helpers
  def model_biggest(list), do: List.last(Enum.sort(list))

  # Generators
end
