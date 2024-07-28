defmodule EmployeeTest do
  use ExUnit.Case
  use PropCheck
  alias Bday.Employee

  # Properties
  property "check that leading space is fixed" do
    forall map <- raw_employee_map() do
      emp = Employee.adapt_csv_result_shim(map)
      strs = Enum.filter(Map.keys(emp) ++ Map.values(emp), &is_binary/1)
      Enum.all?(strs, &(String.first(&1) != " "))
    end
  end

  property "check that the date is formatted right" do
    forall map <- raw_employee_map() do
      case Employee.adapt_csv_result_shim(map) do
        %{date_of_birth: %Date{}} ->
          true

        _ ->
          false
      end
    end
  end

  property "check access through handle" do
    forall maps <- non_empty(list(raw_employee_map())) do
      handle =
        maps
        |> Bday.Csv.encode()
        |> Bday.Employee.from_csv()

      filtered = Employee.filter_birthday(handle, ~D[1900-01-01])
      full_list = Employee.fetch(handle)

      for x <- full_list do
        Employee.first_name(x)
        Employee.last_name(x)
        Employee.date_of_birth(x)
        Employee.email(x)
      end

      is_list(Employee.fetch(filtered))
    end
  end

  # Generators
  def raw_employee_map() do
    let prop_list <- [
          {"last_name", CsvTest.field()},
          {" first_name", whitespaced_text()},
          {" date_of_birth", text_date()},
          {" email", whitespaced_text()}
        ] do
      Map.new(prop_list)
    end
  end

  defp whitespaced_text() do
    let(txt <- CsvTest.field(), do: " " <> txt)
  end

  defp text_date() do
    rawdate = {choose(1900, 2020), choose(1, 12), choose(1, 31)}
    # only generate valid dates

    date =
      such_that(
        {y, m, d} <- rawdate,
        when: {:error, :invalid_date} != Date.new(y, m, d)
      )

    let {y, m, d} <- date do
      IO.chardata_to_string(:io_lib.format(" ~w/~2..0w/~2..0w", [y, m, d]))
    end
  end
end
