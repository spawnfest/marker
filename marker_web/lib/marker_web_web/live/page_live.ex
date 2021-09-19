defmodule MarkerWebWeb.PageLive do
  use MarkerWebWeb, :live_view
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, markdown: nil, result: nil, preview_hidden: true)}
  end

  @impl true
  def handle_event("to_html", %{"m" => markdown}, socket) do
    Logger.info("got event #{markdown}")
    {:ok, parsed_markdown} = parse_markdown(markdown)

    {
      :noreply,
      assign(
        socket,
        markdown: "",
        result: parsed_markdown,
        generated_html: generate_html(parsed_markdown)
      )
    }
  end

  @impl true
  def handle_event("toggle-preview", %{"state" => "true"},  socket) do
    {:noreply, assign(socket, preview_hidden: false)}
  end

  @impl true
  def handle_event("toggle-preview", %{"state" => "false"},  socket) do
    {:noreply, assign(socket, preview_hidden: true)}
  end

  defp parse_markdown(markdown) do
    :marker.markdown(markdown |> to_charlist())
  end

  defp generate_html(parsed_markdown) do
    :render.html(parsed_markdown)
  end
end
