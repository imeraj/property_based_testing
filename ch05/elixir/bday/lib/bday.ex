defmodule Bday do
  @moduledoc """
  Documentation for `Bday`.
  """

  def run(path) do
    set =
      path
      |> File.read!()
      |> Bday.Employee.from_csv()
      |> Bday.Employee.filter_birthday(DateTime.to_date(DateTime.utc_now()))
      |> Bday.Employee.fetch()

    for employee <- set do
      employee
      |> Bday.MailTpl.full()
      |> send_email()
    end

    :ok
  end

  defp send_email({to, _, _}) do
    IO.puts("sent birthday email to #{to}")
  end
end
