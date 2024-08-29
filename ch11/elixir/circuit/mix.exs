defmodule Circuit.MixProject do
  use Mix.Project

  def project do
    [
      app: :circuit,
      version: "0.1.0",
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:circuit_breaker, git: "https://github.com/klarna/circuit_breaker.git", tag: "1.0.1"},
      {:propcheck, "~> 1.4.1", only: [:test, :dev]}
    ]
  end
end
