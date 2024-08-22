defmodule Bookstore.App do
  @moduledoc false

  use Application

  def start(_type, _args) do
    Bookstore.Sup.start_link()
  end

  def stop(_state) do
    :ok
  end
end
