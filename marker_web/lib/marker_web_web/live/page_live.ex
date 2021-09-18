defmodule MarkerWebWeb.PageLive do
  use MarkerWebWeb, :live_view
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, markdown: "", result: "")}
  end

  @impl true
  def handle_event("to_html", %{"m" => markdown}, socket) do
    Logger.info("got event #{markdown}")
    {:noreply, assign(socket, markdown: "", result: to_html(markdown))}
  end

  defp to_html(markdown) do
    :marker.markdown(markdown |> to_charlist())
  end
end
