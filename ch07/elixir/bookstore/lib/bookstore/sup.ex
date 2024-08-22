defmodule Bookstore.Sup do
  @moduledoc false
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    Bookstore.DB.load_queries()
    Supervisor.init([], strategy: :one_for_one)
  end
end
