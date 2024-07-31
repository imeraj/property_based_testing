defmodule CheckoutTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "sums without specials" do
    forall {item_list, expected_price, price_list} <- item_price_list() do
      expected_price == Checkout.total(item_list, price_list, [])
    end
  end

  # Generators
  def item_price_list do
    let price_list <- price_list() do
      let {item_list, expected_price} <- item_list(price_list) do
        {item_list, expected_price, price_list}
      end
    end
  end

  defp price_list do
    let price_list <- non_empty(list({non_empty(utf8()), integer()})) do
      price_list
      |> Enum.sort()
      |> Enum.dedup_by(&elem(&1, 0))
    end
  end

  defp item_list(price_list) do
    sized(size, item_list(size, price_list, {[], 0}))
  end

  defp item_list(0, _, acc), do: acc

  defp item_list(n, price_list, {item_list, expected_price}) do
    let {item, price} <- elements(price_list) do
      item_list(n - 1, price_list, {[item | item_list], price + expected_price})
    end
  end
end
