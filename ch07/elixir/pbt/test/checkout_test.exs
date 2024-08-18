defmodule CheckoutTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "sums without specials" do
    forall {item_list, expected_price, price_list} <- item_price_list() do
      expected_price == Checkout.total(item_list, price_list, [])
    end
  end

  property "sums without specials (with metrics)", [:verbose] do
    forall {item_list, expected_price, price_list} <- item_price_list() do
      collect(
        expected_price == Checkout.total(item_list, price_list, []),
        bucket(length(item_list), 10)
      )
    end
  end

  property "sums including specials" do
    forall {item_list, expected_price, prices, specials} <- item_price_special() do
      expected_price == Checkout.total(item_list, prices, specials)
    end
  end

  property "negative testing for expected results" do
    forall {item_list, price_list, special_list} <- lax_lists() do
      try do
        is_integer(Checkout.total(item_list, price_list, special_list))
      rescue
        e in [RuntimeError] ->
          String.starts_with?(e.message, "unknown item:") ||
            e.message == "invalid list of specials" ||
            e.message == "invalid list of prices"

        _ ->
          false
      end
    end
  end

  property "list of items with duplicates" do
    forall price_list <- dupe_list() do
      false == Checkout.valid_price_list(price_list)
    end
  end

  # Generators
  defp item_price_special do
    let price_list <- price_list() do
      let special_list <- special_list(price_list) do
        let {{regular_items, regular_expected}, {special_items, special_expected}} <-
              {regular_gen(price_list, special_list), special_gen(price_list, special_list)} do
          {Enum.shuffle(regular_items ++ special_items), regular_expected + special_expected,
           price_list, special_list}
        end
      end
    end
  end

  defp special_list(price_list) do
    items = for {name, _} <- price_list, do: name

    let specials <- list({elements(items), choose(2, 5), non_neg_integer()}) do
      specials |> Enum.sort() |> Enum.dedup_by(&elem(&1, 0))
    end
  end

  defp regular_gen(price_list, special_list) do
    regular_gen(price_list, special_list, [], 0)
  end

  defp regular_gen([], _special_list, item_list, total_price) do
    {item_list, total_price}
  end

  defp regular_gen([{item, cost} | prices], specials, items, price) do
    count_gen =
      case List.keyfind(specials, item, 0) do
        {_, limit, _} -> choose(0, limit - 1)
        _ -> non_neg_integer()
      end

    let count <- count_gen do
      regular_gen(
        prices,
        specials,
        let(v <- vector(count, item), do: v ++ items),
        cost * count + price
      )
    end
  end

  defp special_gen(_, special_list) do
    special_gen(special_list, [], 0)
  end

  defp special_gen([], items, price), do: {items, price}

  defp special_gen([{item, count, cost} | specials], items, price) do
    let multiplier <- non_neg_integer() do
      special_gen(
        specials,
        let(v <- vector(count * multiplier, item), do: v ++ items),
        cost * multiplier + price
      )
    end
  end

  defp item_price_list do
    let price_list <- price_list() do
      let {item_list, expected_price} <- item_list(price_list) do
        {item_list, expected_price, price_list}
      end
    end
  end

  defp price_list do
    let price_list <- non_empty(list({non_empty(utf8()), non_neg_integer()})) do
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

  defp lax_lists() do
    known_items = ["A", "B", "C"]
    maybe_known_item_gen = elements(known_items ++ [utf8()])

    {list(maybe_known_item_gen), list({maybe_known_item_gen, non_neg_integer()}),
     list({maybe_known_item_gen, non_neg_integer(), non_neg_integer()})}
  end

  defp dupe_list() do
    let items <- non_empty(list(utf8())) do
      vector(length(items) + 1, {elements(items), non_neg_integer()})
    end
  end

  # Helpers
  defp bucket(n, unit), do: div(n, unit) * unit
end
