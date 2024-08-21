defmodule Bookstore.MixProject do
  use Mix.Project

  def project do
    [
      app: :bookstore,
      version: "0.1.0",
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript_config()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Bookstore.App, []},
      env: [
        pg: [
          # Single quotes are important
          user: ~c"postgres",
          password: ~c"postgres",
          database: ~c"bookstore_db",
          port: 5432,
          # not for tests!
          ssl: false
        ]
      ]
    ]
  end

  defp escript_config do
    [main_module: Bookstore.Init, app: nil]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:eql, "~> 0.1.2", manager: :rebar3},
      {:pgsql, "~> 26.0"},
      {:propcheck, "~> 1.1", only: [:test, :dev]}
    ]
  end
end
