defmodule Checkout do
  @moduledoc false

  def total(item_list, price_list, _specials) do
    for item <- item_list do
      elem(List.keyfind(price_list, item, 0), 1)
    end
    |> Enum.sum()
  end
end
