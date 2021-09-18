defmodule MarkerWeb.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      MarkerWebWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: MarkerWeb.PubSub},
      # Start the Endpoint (http/https)
      MarkerWebWeb.Endpoint
      # Start a worker by calling: MarkerWeb.Worker.start_link(arg)
      # {MarkerWeb.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MarkerWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    MarkerWebWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
