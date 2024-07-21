defmodule CsvTest do
  use ExUnit.Case
  use PropCheck
  alias Bday.Csv, as: Csv

  ## Properties ##

  property "roundtrip encoding/decoding" do
    forall maps <- csv_source() do
      maps == Csv.decode(Csv.encode(maps))
    end
  end

  ## Generators ##

  def csv_source() do
    let size <- pos_integer() do
      let keys <- header(size + 1) do
        list(entry(size + 1, keys))
      end
    end
  end

  def entry(size, keys) do
    let vals <- record(size) do
      Map.new(Enum.zip(keys, vals))
    end
  end

  def header(size) do
    vector(size, name())
  end

  def record(size) do
    vector(size, field())
  end

  def name() do
    field()
  end

  def field() do
    oneof([unquoted_text(), quotable_text()])
  end

  # using charlists for the easy generation
  def unquoted_text() do
    let chars <- list(elements(textdata())) do
      to_string(chars)
    end
  end

  def quotable_text() do
    let chars <- list(elements(~c'\r\n",' ++ textdata())) do
      to_string(chars)
    end
  end

  def textdata() do
    ~c"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" ++
      ~c":;<=>?@ !#$%&'()*+-./[\\]^_`{|}~"
  end
end
