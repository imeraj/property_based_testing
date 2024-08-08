defmodule Checkout do
  @moduledoc false

  def total(item_list, price_list, specials) do
    if not valid_price_list(price_list),
      do: raise(RuntimeError, message: "invalid list of prices")

    if not valid_special_list(specials),
      do: raise(RuntimeError, message: "invalid list of specials")

    counts = count_seen(item_list)
    {counts_left, prices} = apply_specials(counts, specials)
    prices + apply_regular(counts_left, price_list)
  end

  def valid_price_list(prices) do
    unique =
      prices
      |> Enum.sort()
      |> Enum.dedup_by(&elem(&1, 0))

    length(prices) == length(unique)
  end

  defp valid_special_list([]), do: true

  defp valid_special_list(specials) do
    Enum.all?(specials, fn {_, x, _} -> x != 0 end)
  end

  defp count_seen(item_list) do
    count = &(&1 + 1)

    Enum.reduce(item_list, %{}, fn item, m ->
      Map.update(m, item, 1, count)
    end)
  end

  defp apply_specials(items, specials) do
    Enum.map_reduce(items, 0, fn {name, count}, price ->
      case List.keyfind(specials, name, 0) do
        nil ->
          {{name, count}, price}

        {_, needed, value} ->
          {{name, rem(count, needed)}, value * div(count, needed) + price}
      end
    end)
  end

  defp apply_regular(items, price_list) do
    Enum.sum(
      for {name, count} <- items do
        count * cost_of_item(name, price_list)
      end
    )
  end

  defp cost_of_item(name, price_list) do
    case List.keyfind(price_list, name, 0) do
      nil -> raise RuntimeError, message: "unknown item: #{name}"
      {_, price} -> price
    end
  end
end
