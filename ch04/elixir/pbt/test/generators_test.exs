defmodule GeneratorsTest do
  use ExUnit.Case
  use PropCheck

  # Properties
  property "find all keys in a map even when dupes are used", [:verbose] do
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
    forall bin <- resize(150, binary()) do
      #    test                   metric
      collect(is_binary(bin), to_range(10, byte_size(bin)))
    end
  end

  property "aggregate", [:verbose] do
    suits = [:club, :diamond, :heart, :spade]

    forall hand <- vector(5, {oneof(suits), choose(1, 13)}) do
      aggregate(true, hand)
    end
  end

  property "fake escaping test showcasing aggregation", [:verbose] do
    forall str <- utf8() do
      aggregate(escape(str), classes(str))
    end
  end

  property "profile 1", [:verbose] do
    forall profile <- [
             name: resize(10, utf8()),
             age: pos_integer(),
             bio: resize(350, utf8())
           ] do
      name_len = to_range(10, String.length(profile[:name]))
      bio_len = to_range(350, String.length(profile[:bio]))
      aggregate(true, name: name_len, bio: bio_len)
    end
  end

  # Helpers
  defp key(), do: oneof([range(1, 10), integer()])
  defp val(), do: term()

  defp escape(_), do: true

  defp classes(str) do
    l = letters(str)
    n = numbers(str)
    p = punctuation(str)
    o = String.length(str) - (l + n + p)

    [
      {:letters, to_range(5, l)},
      {:numbers, to_range(5, n)},
      {:punctuation, to_range(5, p)},
      {:others, to_range(5, o)}
    ]
  end

  defp letters(str) do
    is_letter = &((&1 >= ?a && &1 <= ?z) or (&1 >= ?A && &1 <= ?Z))
    length(for <<c::utf8 <- str>>, is_letter.(c), do: 1)
  end

  defp numbers(str) do
    is_num = &(&1 >= ?0 && &1 <= ?9)
    length(for <<c::utf8 <- str>>, is_num.(c), do: 1)
  end

  defp punctuation(str) do
    is_punctuation = &(&1 in ~c'.,;:\'"-')
    length(for <<c::utf8 <- str>>, is_punctuation.(c), do: 1)
  end

  defp to_range(m, n) do
    base = div(n, m)
    {base * m, (base + 1) * m}
  end

  # Generators
end
