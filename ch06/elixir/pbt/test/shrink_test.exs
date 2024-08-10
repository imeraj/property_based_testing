defmodule ShrinkTest do
  use PropCheck

  # Paste the module in MIX_ENV=test iex -S mix and type below:
  # :proper_gen.sampleshrink(ShrinkTest.strdatetime())

  def strdatetime do
    let(date_time <- datetime(), do: to_str(date_time))
  end

  defp datetime, do: {date(), time(), timezone()}

  defp date do
    such_that(
      {y, m, d} <- {year(), month(), day()},
      when: :calendar.valid_date(y, m, d)
    )
  end

  defp year do
    shrink(range(0, 9999), [range(1970, 2000), range(1900, 2100)])
  end

  defp month do
    range(1, 12)
  end

  defp day do
    range(1, 31)
  end

  defp time do
    {range(0, 24), range(0, 59), range(0, 60)}
  end

  defp timezone do
    {elements([~c"+", ~c"-"]), shrink(range(0, 99), [range(0, 14), 0]),
     shrink(range(0, 99), [0, 15, 30, 45])}
  end

  def to_str({{y, m, d}, {h, mi, s}, {sign, ho, mo}}) do
    format_str = "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b~s~2..0b:~2..0b"

    :io_lib.format(format_str, [y, m, d, h, mi, s, sign, ho, mo])
    |> to_string()
  end
end
