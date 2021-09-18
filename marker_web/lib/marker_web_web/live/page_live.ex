defmodule MarkerWebWeb.PageLive do
  use MarkerWebWeb, :live_view
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, markdown: "", result: "", rendered_markdown: "")}
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
        rendered_markdown: render_markdown(parsed_markdown)
      )
    }
  end

  defp parse_markdown(markdown) do
    :marker.markdown(markdown |> to_charlist())
  end

  defp render_markdown(parsed_markdown) do
    :render.block_to_html(parsed_markdown)
  end
end
